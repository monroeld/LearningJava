library(tidyverse)
library(readxl)

df <- read_xlsx("20191122_AKR1C3-FFPEvsAllPrep.xlsx")
df$`Adj Cq` = as.numeric(df$`Adj Cq`)
df$Cq = as.numeric(df$Cq)

# test <- df %>% aggregate(by = list(Fluor, Target, Sample), FUN = mean)
#calculate mean Cq
Cq_dfadj <- setNames(aggregate(df$`Adj Cq`,
                            #list what columns to consider in mean calc        
                            by=list(Sample=df$Sample,
                                    Target=df$Target),
                            data=df,
                            FUN=mean, 
                            colnames = "MeanCq"),
                  #set names in order give in "by=list", plus name of calc'd col
                  c("Sample", 
                    "Target", 
                    "MeanCq"))

Cq_df <- setNames(aggregate(df$Cq,
                         #list what columns to consider in mean calc        
                         by=list(Sample=df$Sample,
                                 Target=df$Target),
                         data=df,
                         FUN=mean, 
                         colnames = "MeanCq"),
               #set names in order give in "by=list", plus name of calc'd col
               c("Sample", 
                 "Target", 
                 "MeanCq"))

#make tidy Cq table
Cq_spreadadj <- Cq_dfadj %>% 
  select(Sample, Target, MeanCq) %>% 
  spread(key = Target, value = MeanCq) %>%
  mutate(diff = abs(ABL - AKR1C3)) %>%
  separate(col = Sample, into = c("Sample", "Extraction"), sep = " ", remove = TRUE)

Cq_spread <- Cq_df %>% 
  select(Sample, Target, MeanCq) %>% 
  spread(key = Target, value = MeanCq) %>%
  mutate(diff = abs(ABL - AKR1C3)) %>%
  separate(col = Sample, into = c("Sample", "Extraction"), sep = " ", remove = TRUE)

Cq_spread <- Cq_spread[!is.na(Cq_spread$Extraction), ]
Cq_spreadadj <- Cq_spreadadj[!is.na(Cq_spreadadj$Extraction), ]

Basic <- ggplot() + geom_point(data = Cq_spread,
                      aes(x = ABL, y = AKR1C3, color = factor(Sample))) +
  facet_wrap(~Extraction) + xlim(c(15, 40)) + ylim(c(15, 40)) +
  ggtitle("Non-adjusted Cqs") +
  xlab("ABL Cq") + ylab("AKR1C3 Cq")

Basic_adj <- ggplot() + geom_point(data = Cq_spreadadj,
                                   aes(x = ABL, y = AKR1C3, color = factor(Sample))) +
  facet_wrap(~Extraction) + xlim(c(15, 40)) + ylim(c(15, 40)) +
  ggtitle("Cqs adjusted for Sample Concentration") +
  xlab("ABL Cq") + ylab("AKR1C3 Cq")

ggsave("20191122_CqScatter.pdf",
       arrangeGrob(Basic, Basic_adj, nrow = 2),
       width = 8, height = 8)

