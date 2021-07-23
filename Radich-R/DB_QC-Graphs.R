library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(readxl)
library(tidyxl)
library(stringr)
library(padr)
library(tidyr)

df <- data.frame(read_xlsx("QC_Stats.xlsx"), stringsAsFactors = F)

df$Sample <- factor(df$Sample, levels = c("AML", "75% AML", "25% AML", "Mouse"))

melted <- melt(df, id = "Sample")

# working <- melted %>% filter(variable %in% c("Total", "Complete_Adapter", "Trimmed_Adapter",
#                                              "All_Mapped", "All_On.Target"))

# , "Unique_Mapped"
# write.table(working, file = "intermediate.csv", sep = ",")

# test <- data.frame(read.csv("intermediate.csv", stringsAsFactors=F))
# test$Sample <- factor(test$Sample, levels = c("AML", "75% AML", "25% AML", "Mouse"))
# test$variable <- factor(test$variable, levels = c("Total", "Complete_Adapter",
#                                                   "Trimmed_Adapter", "All_Mapped", "All_On.Target"))


# fracPlot <- ggplot(data = test) +
#   geom_bar(mapping = aes(x = Sample, y = diffs, fill = variable),
#            stat = "identity", position = "fill") +
#   labs(y = "Fraction", fill = "Reads") + ggtitle("Read Fraction by Sample")
# 
# testing <- working %>% filter(variable != "All_On.Target")
# dodgePlot <- ggplot(data = test, aes(x = variable, y = value, fill = Sample)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(y = "Read Count", fill = "Sample", x = "Read Type") +
#   ggtitle("Total Reads per Sample")

# ggsave("fracPlot.png", fracPlot, width = 5, height = 5)
# ggsave("dodgePlot.png", dodgePlot, width = 7, height = 5)



# working2 <- melted %>% filter(variable %in% c("All_Alignment",
#                       "Unique_Alignment", "All_On.Target", "Unique_OnTarget",
#                       "Unique_DNA_Per_GSP2", "On_Target_Deduplication"))

# write.table(working2, file = "intermediate2.csv", sep = ",")
# 
# test2 <- data.frame(read.csv("intermediate2.csv", stringsAsFactors=F))
# test2$Sample <- factor(test2$Sample, levels = c("AML", "75% AML", "25% AML", "Mouse"))
# test2$variable <- factor(test2$variable, levels = c("All_Alignment",
#                                                     "Unique_Alignment", "All_On.Target", "Unique_OnTarget",
#                                                     "Unique_DNA_Per_GSP2", "On_Target_Deduplication"))
# 
# ggplot(data = working2, aes(x = variable, y = value, fill = Sample)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(y = "Read Count", fill = "Sample", x = "Read Type") +
#   ggtitle("Total Reads per Sample")
# 
# ggplot(data = test2) +
#   geom_bar(mapping = aes(x = Sample, y = diffs, fill = variable),
#            stat = "identity", position = "fill") +
#   labs(y = "Fraction", fill = "Reads") + ggtitle("Read Fraction by Sample")




fluff <- df %>% select(-c(All_Alignment, Unique_Alignment))
fluff <- fluff %>% mutate(All_On.Target = All_On.Target * All_Mapped/100,
                          Unique_OnTarget = Unique_OnTarget * Unique_Mapped/100)
meltedfluff <- melt(fluff, id = "Sample")
# write.table(meltedfluff, file = "final.csv", sep = ",")

final <- data.frame(read.csv("final.csv", stringsAsFactors=F))
final$Sample <- factor(final$Sample, levels = c("AML", "75% AML", "25% AML", "Mouse"))
final$variable <- factor(final$variable, levels = c("Total","Complete_Adapter",
                                                    "Trimmed_Adapter","All_Mapped", "All_On.Target",
                                                    "Unique_Mapped","Unique_OnTarget"))

# ggplot(data = final, aes(x = variable, y = value, fill = Sample)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(y = "Read Count", fill = "Sample", x = "Read Type") +
#   ggtitle("Total Reads per Sample")

fracPlot <- ggplot(data = final) +
  geom_bar(mapping = aes(x = Sample, y = diffs, fill = variable),
           stat = "identity", position = "fill") +
  labs(y = "Fraction", fill = "Reads") + ggtitle("Read Fraction by Sample")

# ggsave("fracPlot.png", fracPlot, width = 5, height = 5)


melted <- df %>% select(c(Sample, Unique_DNA_Per_GSP2, On_Target_Deduplication))
melted <- melt(melted, id = "Sample")

gsp <- filter(melted, variable == "Unique_DNA_Per_GSP2")
dedup <- filter(melted, variable == "On_Target_Deduplication")

dodgePlot <- ggplot(data = gsp) +
  geom_bar(mapping = aes(x = variable, y = value, fill = Sample),
           stat = "identity", position = "dodge") +
  labs(y = "Reads") + ggtitle("Unique Reads per GSP2") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") + guides(fill = FALSE)

dodgePlot + theme(legend.position = "none")

dodgePlot2 <- ggplot(data = dedup) +
  geom_bar(mapping = aes(x = variable, y = value, fill = Sample),
           stat = "identity", position = "dodge") +
  labs(y = "Fold") + ggtitle("On-Target Deduplication") +
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("dodgePlot.png", dodgePlot, width = 3, height = 5)
ggsave("dodgePlot2.png", dodgePlot2, width = 4, height = 5)


