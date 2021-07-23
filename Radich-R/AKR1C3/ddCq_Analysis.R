library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

df <- read.csv("20191217_AKR1C3-Run7.csv",
               header = T, skip = 19, stringsAsFactors = F)
df$Cq = as.numeric(df$Cq)
df <- df %>% select(-c(Content, Starting.Quantity..SQ.))

# test <- df %>% aggregate(by = list(Fluor, Target, Sample), FUN = mean)
#calculate mean Cq
Cqdf <- setNames(aggregate(df$Cq,
                            #list what columns to consider in mean calc        
                            by=list(Sample=df$Sample,
                                    Target=df$Target),
                            data=df,
                            FUN=mean, na.rm = T, 
                            colnames = "MeanCq"),
                  #set names in order give in "by=list", plus name of calc'd col
                  c("Sample", 
                    "Target", 
                    "MeanCq"))

#make tidy Cq table
Cq_spread <- Cqdf %>% 
  select(Sample, Target, MeanCq) %>% 
  spread(key = Target, value = MeanCq)


highpos <- c("High_pos", colMeans(Cq_spread[grep("High_pos", Cq_spread$Sample), 2:3]))
lowpos <- c("Low_pos", colMeans(Cq_spread[grep("Low_pos", Cq_spread$Sample), 2:3]))
pooledref <- c("Pooled_ref", colMeans(Cq_spread[grep("Pooled_ref", Cq_spread$Sample), 2:3]))

collapsed <- Cq_spread[setdiff(1:nrow(Cq_spread), grep("_", Cq_spread$Sample)), ]
collapsed <- rbind(collapsed, highpos, lowpos, pooledref, stringsAsFactors = F)
collapsed$ABL <- as.numeric(collapsed$ABL)
collapsed$AKR1C3 <- as.numeric(collapsed$AKR1C3)

ggplot(collapsed) +
  geom_point(aes(x = ABL, y = AKR1C3, color = factor(Sample))) +
  xlim(c(15, 40)) + ylim(c(15, 40))




# ggsave("20191122_CqScatter.pdf",
#        arrangeGrob(Basic, Basic_adj, nrow = 2),
#        width = 8, height = 8)

