require(ggplot2)
require(dplyr)
require(plyr)
require(RColorBrewer)

data <- read.csv("SSASF.csv", sep = ",", stringsAsFactors = FALSE)
data <- data[, 2:4]

data$DATE.RECEIVED <- as.Date(data$DATE.RECEIVED, format = "%d/%m/%Y")

colnames(data) <- c("Recipient", "Date", "Disbursement")

data$Disbursement <- gsub(",", "", data$Disbursement)

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