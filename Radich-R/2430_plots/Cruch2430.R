library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(readxl)
library(tidyxl)
library(stringr)
library(padr)
library(tidyr)
library(ggVennDiagram)

df <- data.frame(read_xlsx("p2430_Archer_Variants_ClinVar.xlsx"), stringsAsFactors = F)

vars <- data.frame(read_xlsx("2020-08-25_Heme-Variants-List.xlsx"), stringsAsFactors = F)

# Pare down the data, then add new columns corresponding to Gene-Protein or Gene-Genome
df <- df %>% select(c(Sample, Symbol, HGVSp, HGVSc, AF)) %>%
  mutate(Protein = paste(Symbol, HGVSp),
         Genomic = paste(Symbol, HGVSc))

# Extract the patient ID
df <- df %>% mutate(Sample = str_extract(Sample, "[_][0-9a-zA-Z]*[_]"))

df <- df %>% mutate(Patient = substr(Sample, 2, 3),
                    Time = substr(Sample, 4,4))

# Remove choss from NM_ID's formatting
df <- df %>% mutate(NM_ID = str_extract(HGVSc, "^[0-9a-zA-Z_]*")) %>%
  separate(HGVSc, sep = "[:]", into = c("Filler", "Variant"))

# 7/16/2021: For the new info, *only* read in the rows with red text
#            Also, ignore the RAD21 gene entirely. Likely a sequencing/filtering error


# Cecilia only cares about certain mutations.  Pull them from her highlighted sheet

# # They're highlighted, which is a pain to extract
# test2 <- test$local_format_id[-1]
# test2 <- test %>% filter(local_format_id > 8) %>% filter(local_format_id != 10)
# 
# # Get a vector of genomic events
# test3 <- unlist(test2$character_formatted)
# test3 <- test3[!is.na(test3)]
# names(test3) <- NULL
# 
# # Pull out the names
# CYgenes <- filter(test, col==1)$character[test2$row]
# genename <- as.character(data.frame(strsplit(CYgenes, " "), stringsAsFactors = F)[1, ])
# aachange <- as.character(data.frame(strsplit(test3, ", N"), stringsAsFactors = F)[1, ])
# genomic <- as.character(data.frame(strsplit(test3, ", "), stringsAsFactors = F)[2, ])
# 
# # Glom it together
# CYgenes <- data.frame(cbind(genename, aachange, genomic), stringsAsFactors = F)
# 
# CYgenes <- CYgenes %>% separate(genomic, into = c("NM_Transcript", "Variant"), sep = "[:]")
# 
# CYgenes <- CYgenes %>% mutate(NM_ID = str_extract(NM_Transcript, "^[0-9a-zA-Z_]*"))

# CYpresent <- CYgenes$genename[CYgenes$NM_ID %in% df$NM_ID]
# 
# CYdf <- filter(df, Symbol %in% CYpresent) %>% select(-c(Filler))


# Let's do this again.  Cecilia deleted things and I copied it over from a ppt

cydf <- data.frame(read_xlsx("p2430-Samples_Cecilia-Curated.xlsx"), stringsAsFactors = F)

# Pull the useful text from her curation
cydf <- cydf %>% mutate(Symbol = str_extract(Symbol, "[0-9a-zA-Z]*")) %>%
                 mutate(GeneEvent = str_trim(GeneEvent, side = "right"))
cydf <- cydf %>% mutate(Genomic = paste(Symbol, GeneEvent))

# There's a weird unicode tag on one line and I can't figure out where it's coming from

cydf[28, 3] <- str_replace(cydf[28, 3],"\\u200b","")


# Use Cecilia's list to filter the data

filtered <- semi_join(df, cydf, by = c("Patient", "Genomic"))



# For each patient...

sapply(unique(filtered$Patient), function(X) {
  
  # working <- filter(CYdf, Patient == X)]
  working <- filter(filtered, Patient == X)
  
  
  # Only genetic events that show up in multiple timepoints
  vals <- working %>% group_by(Genomic) %>% dplyr::summarise(n()) %>% filter(`n()` != 1)
  working <- working %>% filter(Genomic %in% vals$Genomic)

  
  # Add zero values
  working <- working %>% complete(Genomic, Time, fill = list(AF = 0))
  
  # test <- working %>% group_by(Genomic) %>% filter(AF != 0) %>% 
  #   mutate(cutoff = ifelse(any(AF < 0.05), 1, 2))
  # test <- filter(test, cutoff == 1)
  
  # working <- working %>% mutate(cutoff = ifelse(Genomic %in% test$Genomic, 1, 2))
  
  # Add the rest of the info
  working <- fill(working, c(Symbol, Sample
                             # cutoff
                             ))
  
  # Adjust y-axis accordingly
  ymax <- ifelse(max(working$AF) > 0.5, 1, 0.5)
  
  # Generate plot and save it
  finalPlot <- ggplot(data = working, aes(x = Time, y = AF,
                                          color = factor(Genomic),
                                          group = factor(Genomic)
                                          # alpha = factor(cutoff),
                                          # linetype = factor(cutoff)
                                          )) +
    geom_point(size = 2) + geom_path(size = 1) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    # scale_alpha_manual(values = c(0.4, 1)) +
    # scale_linetype_manual(values = c("dashed", "solid")) +
    labs(title = paste("Patient", X), color = "Genomic Event",
         x= "Time Point", y = "Allele Fraction") + ylim(c(0, ymax))
  
  ggsave(filename = paste0("Patient_", X, ".png"), plot = finalPlot, width = 7, height = 4)
  
}
)

# thepage = readLines('https://bioinfogp.cnb.csic.es/tools/venny/')

