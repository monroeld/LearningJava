require(plyr)
require(dplyr)
require(ggplot2)

# Read, get columns we care about
df <- read.csv("SSASF 2017 Donor List.csv", skip = 5, header = TRUE, stringsAsFactors = FALSE)
df <- df[2:nrow(df), c(1,3)]
colnames(df) <- c("Date", "Amount")
workingdf <- df

# Fix the date
workingdf$Date <- as.Date(gsub("^[0-9]", "2", as.Date(workingdf$Date, format = "%m/%d/%Y")))

# Sub commas, change from dollars
workingdf$Amount <- gsub("\\$", "", workingdf$Amount)
workingdf$Amount <- as.numeric(gsub("\\,", "", workingdf$Amount))
workingdf <- workingdf[!is.na(workingdf$Amount), ]
workingdf <- workingdf[1:(nrow(workingdf)-1), ]

workingdf$Total <- cumsum(workingdf$Amount)

workingdf <- separate(workingdf, Date, c("Year", "Month", "Day"), remove = FALSE)

# Breakdown by year
ggplot(data = (workingdf %>% group_by(Year) %>% summarise(sum(Amount))),
              aes(x = Year, y = `sum(Amount)`)) + geom_bar(stat = "identity") +
              ylab("Amount raised ($)")


