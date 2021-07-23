library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(readxl)
library(stringr)
library(padr)
library(tidyr)

sequence <- "tttgtttgcactgttgttggggtcagggacagtgattaagataaatttctaattgcagTCTATACGAGATACTCCAGCCAAAAATGCACAAAAGTCAAATCAGAATGGAAAAGACTCAAAACCATCATCAACACCAAGATCAAAAgtaagtggctacatttacacgtgggtctcattgatctagttggggaaaaagattct"

# Turn into vector
sequence <- tolower(sequence)
sequence <- el(strsplit(sequence, ""))

# Primers and probes are typically in the neighborhood of 20bp, so use that much leeway on each end
test <- sapply(1:(length(sequence)-20), function(x)
              sum(sequence[x:(x+20)] %in% c("c", "g")))

# Remember that Pos isn't the exact position!
test <- data.frame(cbind(c(11:length(sequence)-10), test), stringsAsFactors = F)
colnames(test) <- c("Pos", "Count")
test <- test %>% mutate(Frac = Count/20)

# Just a basic plot of %GC by position
ggplot(data = test, aes(x = Pos, y = Frac)) +
  geom_point() + ylim(c(0, 1)) + geom_hline(yintercept = 0.3, color = "red") +
  geom_hline(yintercept = 0.8, color = "red") + geom_hline(yintercept = 0.4, linetype = "dashed")
