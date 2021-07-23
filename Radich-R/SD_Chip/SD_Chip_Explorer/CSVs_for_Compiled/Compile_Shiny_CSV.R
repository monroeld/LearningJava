# This script writes the compiled CSV for the shiny app.  Eventually it'll
#   probably get built into the plotting script


# Initialize libraries

require(plyr)
require(dplyr)
require(tidyr)
require(reshape2)

# Make sure to change basepath and the pertinent subfolders.
#   The paste(basepath, ) method makes it harder to overwrite data from other
#   runs, but make sure you change savedate and mastername as well, as the
#   former affects serveroutput

histalpha <- 0.45
scatteralpha <- histalpha * 2/3
fluorlist <- c("FAM", "HEX", "Cy5")

welltotal <- 1024

basepath <- getwd()

serverinput = basepath
serveroutput = basepath

# These functions are used by multiple chunks
lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}
differentiate <- function(ylist, xlist) {
  c(unlist(sapply((2:length(ylist)),
                  function(x) c((ylist[x]-ylist[x-1]) / (xlist[x]-xlist[x-1])))))
}

linksubset <- 0
rowcolremove <- FALSE




# Read in ALL DATA and modify it so it's ready for subsetting

filelist <- list.files(serverinput, pattern = "Compiled.csv")

Alldata <- do.call(rbind, lapply(filelist, function(x)
  as.data.frame(read.csv(x, head=TRUE,sep = ",",
                         stringsAsFactors = FALSE))))

Alldata <- mutate(Alldata, FAMAID = FAMIntDen/FAMXArea)
Alldata <- mutate(Alldata, HEXAID = HEXIntDen/HEXXArea)
Alldata <- mutate(Alldata, Cy5AID = Cy5IntDen/Cy5XArea)


# CellGroup lets us call things in the pie chart more easily (and in general)
Alldata <- mutate(Alldata, CellGroup=CellCount)
Alldata <- Alldata %>% mutate(CellGroup=replace(CellGroup,
                                                CellGroup>=2,"2 or more"))
Alldata <- Alldata %>% mutate(CellGroup=replace(CellGroup,
                                                CellCount=="N/A", "N/A"))

Alldata <- mutate(Alldata, ArrayContents = paste0("Array ", Array, " (",
                                                  RefMat, ", ",
                                                  Triton, "% Triton, ",
                                                  Tween, "% Tween)"))
Alldata <- mutate(Alldata, ArrayID = paste0(RunDate, "_M",
                                            Master, "_A",
                                            Array))

Alldata <- mutate(Alldata, Row = (Well-1) %/% 64 + 1)
Alldata <- mutate(Alldata, Column = (Well-1) %% 64 + 1)

ArrayIDs <- unique(Alldata$ArrayID)


# Plot the density curve of an array, then find the inflection point.
# Add this value to plotdata.
# For this plot, I'm defining the inflection point as the maximum value
#   of the density's second derivative AFTER maximum density value
# Use the maximum value of the density's second derivative
#   larger the maximum value of the density to draw the line

max2deriv <- function(arrayID, fluor) {
  dendata <- subset(Alldata, ArrayID == arrayID)
  array <- dendata$Array[1]
  fluorcolumn <- grep(paste0(fluor,"AID"), colnames(dendata))
  fluorden <- density(dendata[, fluorcolumn], na.rm = TRUE)
  
  den <- data.frame(cbind(fluorden$x, fluorden$y))
  colnames(den) <- c("X", "Y")
  denmax <- den$X[match(max(den$Y, na.rm = TRUE), den$Y)]
  
  # Data frame of X and first derivative
  dydx <- data.frame(cbind(den$X[2:length(den$X)], differentiate(den$Y, den$X)))
  colnames(dydx) <- c("X", "dYdX")
  # Data frame of X and second derivative
  dydx2 <- data.frame(cbind(dydx$X[2:length(dydx$X)], differentiate(dydx$dYdX, dydx$X)))
  colnames(dydx2) <- c("X", "dYdX2")
  dydx2$X[match(max(subset(dydx2, X > denmax)$dYdX2, na.rm = TRUE), dydx2$dYdX2)]
}

assignlimits <- function(arrayID, fluor) {
  working <- subset(Alldata, ArrayID == arrayID)
  working <- mutate(working, limcol = max2deriv(arrayID, fluor))
  names(working)[names(working) == "limcol"] <- paste0(fluor, "lim")
  return(working)
}

FAMlims <- do.call(rbind, lapply(ArrayIDs, function(x)
  assignlimits(x, "FAM")))$FAMlim
HEXlims <- do.call(rbind, lapply(ArrayIDs, function(x)
  assignlimits(x, "HEX")))$HEXlim
Cy5lims <- do.call(rbind, lapply(ArrayIDs, function(x)
  assignlimits(x, "Cy5")))$Cy5lim
Alldata <- cbind(Alldata, FAMlims, HEXlims, Cy5lims)


callzygs <- function(arrayID) {
  data <- subset(Alldata, ArrayID == arrayID)
  ampfluorcol <- grep(paste0(data$AmpFluor[1], "AID"), colnames(data))
  amplimscol <- grep(paste0(data$AmpFluor[1], "lims"), colnames(data))
  wtfluorcol <- grep(paste0(data$WTFluor[1], "AID"), colnames(data))
  wtlimscol <- grep(paste0(data$WTFluor[1], "lims"), colnames(data))
  mutfluorcol <- grep(paste0(data$MUTFluor[1], "AID"), colnames(data))
  mutlimscol <- grep(paste0(data$MUTFluor[1], "lims"), colnames(data))
  
  data$Zygosity[(data$CellGroup == 1 | data$CellGroup == "N/A") &
                  data[, ampfluorcol] >= data[, amplimscol] &
                  data[, wtfluorcol] >= data[, wtlimscol] &
                  data[, mutfluorcol] < data[, mutlimscol]] <- "WT"
  wt <- sum(data$Zygosity == "WT", na.rm = T)
  
  data$Zygosity[(data$CellGroup == 1 | data$CellGroup == "N/A") &
                  data[, ampfluorcol] >= data[, amplimscol] &
                  data[, mutfluorcol] >= data[, mutlimscol] &
                  data[, wtfluorcol] < data[, wtlimscol]] <- "MUT"
  mut <- sum(data$Zygosity == "MUT", na.rm = T)
  
  data$Zygosity[(data$CellGroup == 1 | data$CellGroup == "N/A") &
                  data[, ampfluorcol] >= data[, amplimscol] &
                  data[, mutfluorcol] >= data[, mutlimscol] &
                  data[, wtfluorcol] >= data[, wtlimscol]] <- "HET"
  het <- sum(data$Zygosity == "HET", na.rm = T)
  
  data$Zygosity <- factor(data$Zygosity, level=c("WT", "HET", "MUT"))
  
  return(data)
}


Alldata <- do.call(rbind, lapply(ArrayIDs, function(x)
                    rbind(callzygs(x))))

write.csv(Alldata, "Compiled_Shiny_Data.csv")