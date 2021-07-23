library(tidyverse)
library(reshape2)

df <- read.csv("Spot_CSV.csv", sep = ",", stringsAsFactors = F)

df <- df[, 3:7]
colnames(df) <- c("wbc", "refmat", "spot_date", "conc", "integrity")

# Summarize concentrations and plot 'em

dfConc <- df %>% group_by(refmat) %>%
  summarise(mean = mean(conc), stdev = sd(conc))

concplot <- ggplot() +
  geom_jitter(data = df,
              aes(x = refmat, y = conc, color = refmat),
              width = 0.1) +
  geom_errorbar(data = dfConc,
               aes(x = refmat, ymin = mean-stdev, ymax = mean+stdev),
               stat = "identity", width = 0.3) +
  geom_errorbar(data = dfConc, # Add another with just mean
                aes(x = refmat, ymin = mean, ymax = mean),
                stat = "identity", width = 0.2, size = 1.5) +
  ylim(0, 800) + theme_bw()


# Integrity matters too

dfInt <- df %>% group_by(refmat) %>%
  summarise(mean = mean(integrity), stdev = sd(integrity))

intplot <- ggplot() +
  geom_jitter(data = df,
              aes(x = refmat, y = integrity, color = refmat),
              width = 0.1) +
  geom_errorbar(data = dfInt,
                aes(x = refmat, ymin = mean-stdev, ymax = mean+stdev),
                stat = "identity", width = 0.3) +
  geom_errorbar(data = dfInt, # Add another with just mean
                aes(x = refmat, ymin = mean, ymax = mean),
                stat = "identity", width = 0.2, size = 1.5) +
  ylim(0, 10) + theme_bw()

df <- df %>% mutate(age = as.numeric(Sys.Date() - 
                      as.Date(df$spot_date, format = "%d-%b-%y")))

ggplot(df) + geom_point(aes(x = age, y = conc, color = refmat)) + theme_bw()

ggplot(df) + geom_point(aes(x = wbc, y = conc, color = refmat)) + theme_bw()

ggplot(df) + geom_point(aes(x = age, y = integrity, color = refmat)) + theme_bw()

ggplot(df) + geom_point(aes(x = age, y = conc/wbc, color = refmat)) + theme_bw()



ggplot(dfConc) + geom_bar(aes(x = refmat, y = mean),
                          stat = "identity", alpha = 0.5) +
  geom_errorbar(data = dfConc,
                aes(x = refmat, ymin = mean-stdev, ymax = mean+stdev),
                stat = "identity") + theme_bw()

# Emery has different conc values for two of the high ones (297, 82). Re-qubit at some point
