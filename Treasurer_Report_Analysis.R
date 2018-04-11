library(tidyverse)
library(readxl)

files <- list.files()

wd <- getwd()

allFiles <- sapply(files, function(x)
                    c(unlist(list.files(paste(wd, x, sep = .Platform$file.sep)))))

# The first 3 years have summary excel files (two, because 2009/2010 are combined)
summ2010 <- read_excel(paste(wd, "2010", fileNames$`2010`[1], sep = .Platform$file.sep), skip = 3, trim_ws=TRUE)
summ2010 <- summ2010 %>% select(-c(X__1, Total, Notes))
colnames(summ2010) <- c("Date", "Donor", "Amount")
summ2010 <- summ2010[1:(match(TRUE, is.na(summ2010$Donor))-1), ] # Stop at first whitespace

summ2011 <- read_excel(paste(wd, "2011", fileNames$`2011`[1], sep = .Platform$file.sep), skip = 4, trim_ws=TRUE)
summ2011 <- summ2011 %>% select(Letter, X__1, Gift)
colnames(summ2011) <- c("Date", "Donor", "Amount")
summ2011 <- summ2011[1:(match(TRUE, is.na(summ2011$`Donor`))-1), ] # Stop at first whitespace


fileList <- lapply(files, function(x) as.data.frame(read_excel(x, trim_ws=TRUE, skip = 2)))

# Find index of "Fidelity" and split above/below

cash_changes <- fileList[[1]][1:grep("Fidelity", fileList[[1]]$Description), ] %>%
  select(-Notes) %>% mutate(Balance = as.numeric(Balance)) %>%
  filter(!is.na(Description) & !is.na(Balance) & Description != "Beginning Balance")
Fidelity <- fileList[[1]][grep("Fidelity", fileList[[1]]$Description):nrow(fileList[[1]]), ] %>%
  select(-Notes) %>% 
test <- test[, !all(is.na(test))]

test[complete.cases(test)]
