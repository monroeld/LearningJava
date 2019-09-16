require(ggplot2)
require(dplyr)
require(plyr)
require(RColorBrewer)
library(tidyverse)

data <- read.csv("Bursaries_2013-Present.csv", sep = ",", stringsAsFactors = FALSE)
data <- data[, 3:5]

colnames(data) <- c("Scholar", "Date", "ZAR")

test <- data %>% mutate(Date, as.Date(Date, format = "%Y/%m"))

data$DATE.RECEIVED <- as.Date(data$DATE.RECEIVED, format = "%d/%m/%Y")
data <- 

colnames(data) <- c("Recipient", "Date", "Disbursement")

data$Disbursement <- gsub(",", "", data$Disbursement)

# 



data <- do.call(rbind, lapply(unique(data$Recipient), function(x) {
        working <- subset(data, Recipient == x)
        working <- working %>% arrange(Date) %>% mutate(Total = cumsum(Disbursement))
        return(working)}))

plot <- ggplot(data, aes(x = Date, y = Total, color = factor(Recipient))) +
  geom_point(size = 2) + geom_line() +
  # scale_color_manual(values = palette) +
  labs(color = "Recipient", x = "Year", y = "Total (R)")
plot

ggsave("Scholar_Spendings.png", plot, width = 8, height = 4)
