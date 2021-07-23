options(stringsAsFactors = FALSE)

# This runs off the server currently.  Use the server name to find our data
wd <- getwd()
library(stringr)
serverpath <- paste0(strsplit(wd, "paguirigan_a")[[1]][1])
cardinal <- read.csv(paste0(serverpath, "paguirigan_a/grp/DHArMA/DharmaMetadata_OmicsDataPaths.csv"),
                     header = TRUE, sep = ",")

# Prune to things we care about.  What types of sequencing data, cardinal / omics / dharma ID
trimmedData <- as.data.frame(do.call(rbind, lapply(unique(cardinal$DHArMA.Project.ID), function(x) {
              data <- subset(cardinal, DHArMA.Project.ID == x)
              types <- c(x, data[1, 5],
                         ifelse("DNA sequencing" %in% data[, 6],
                                data[match("DNA sequencing", data[, 6]), 7], ""),
                         "SNP array" %in% data[, 6],
                          "Gene expression array" %in% data[, 6],
                          "DNA sequencing" %in% data[, 6], data[1, 26])
              return(types)
              }
              )))
colnames(trimmedData) <- c("DharmaID", "CardinalID", "targseqID","SNP", "Expression", "targSeq", "Protocol")

# Read in the results of Amy's "ls" after she transferred the sequencing data
filesinAWS <- read.table(paste0(serverpath, "paguirigan_a/grp/DHArMA/CARDINALFilesinAWS.txt"))
colnames(filesinAWS) <- c("dateAdded", "timeAdded", "size", "path")

# Right now we only care about targeted sequencing stuff
targseqdata <- subset(trimmedData, targSeq == TRUE)

# Search filesinAWS for targseqdata's targseqID
foundInAWS <- as.data.frame(do.call(rbind, lapply(unique(targseqdata$DharmaID), function(x) {
              data <- subset(targseqdata, DharmaID == x)
              cardinalID <- data$CardinalID
              
              # Get the ID # out of the filesinAWS path name
              splitpath <- lapply(filesinAWS$path, function(y)
                                  strsplit(y, "CARD")[[1]][2])
              seqID <- lapply(splitpath, function(y)
                                  strsplit(y, "[.]")[[1]][1])
            
              # If the cardinal ID is present in the list of seqID (from foundinAWS)
              if (TRUE %in% grepl(cardinalID, seqID)) {
                # Get the row indices of these matches
                indices <- grep(cardinalID, seqID)
                
                # Split the filesinAWS path name and compare it to the targseqID for both indices
                s3Name <- strsplit(filesinAWS$path[indices[1]], "[.]")[[1]][1]
                AWSstatus <- ifelse(s3Name == data$targseqID, "Match", "NoMatch")
                s3Name <- strsplit(filesinAWS$path[indices[2]], "[.]")[[1]][1]
                AWSstatus <- c(AWSstatus, ifelse(s3Name == data$targseqID, "Match", "NoMatch"))
              } else {
                AWSstatus <- c("cardinalIDNotFound", "cardinalIDNotFound")
              }
              return(AWSstatus)
              }
)))
# Add these results to targseqdata to show which ones are problematic
colnames(foundInAWS) <- c("matchedR1", "matchedR2")
targseqdata <- cbind(targseqdata, foundInAWS)


# test2 <- c(NA, NA, NA, 5, NA)
# library(dplyr)
# trimmedData <- mutate(trimmedData, SNP = replace(SNP, SNP == TRUE, 1))
# trimmedData <- mutate(trimmedData, SNP = replace(SNP, SNP == FALSE, 0))
# trimmedData <- mutate(trimmedData, DNA = replace(DNA, DNA == TRUE, 1))
# trimmedData <- mutate(trimmedData, DNA = replace(DNA, DNA == FALSE, 0))
# trimmedData <- mutate(trimmedData, Expression = replace(Expression, Expression == TRUE, 1))
# trimmedData <- mutate(trimmedData, Expression = replace(Expression, Expression == FALSE, 0))
# 
# paths <- do.call(rbind, lapply(trimmedData$DharmaID, function(x) {
#       data <- subset(cardinal, DHArMA.Project.ID == x)
#       patharray <- c(ifelse("DNA sequencing" %in% data[, 6],
#                             data[match("DNA sequencing", data[, 6]), 10],
#                             NA),
#                      ifelse("Gene expression array" %in% data[, 6],
#                             data[match("SNP array", data[, 6]), 18],
#                             NA),
#                     ifelse("SNP array" %in% data[, 6],
#                        data[match("SNP array", data[, 6]), 18],
#                        NA))
#       return(c(unlist(patharray)))
#       }))
# colnames(paths) <- c("DNA/RNA FastQ", "Expression path", "SNP path")
# 
# seqnums <- cbind(seqnums, paths)
# 
# wd <- getwd()
# library(stringr)
# newpath <- paste0(strsplit(wd, "paguirigan_a")[[1]][1], "paguirigan_a")
# list.files(paste0(newpath, gsub("shared", "SR", notalldata[9, 6])))
# 
# 
# 
# # Data missing one or more values
# notalldata <- subset(trimmedData, SNP == 0 | DNA == 0 | Expression == 0)
