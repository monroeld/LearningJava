require(png)
require(ggplot2)
require(plyr)
require(dplyr)

options(stringsAsFactors = FALSE)

wd <- getwd()

lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}

binaryDims <- function(filename, cutoffVal) {
  testdf <- readPNG(paste(wd, filename, sep = "/"))
  testdf1 <- testdf[, , 1]
  testdf2 <- testdf[, , 2]
  testdf3 <- testdf[, , 3]
  
  grayscale <- testdf1*0.21 + testdf2*0.71 + testdf3*0.07
  rm(testdf1, testdf2, testdf3)
  
  # Invert rows
  grayscale <- grayscale[rev(1:nrow(grayscale)), ]
  
  # Round to 2 digits after decimal
  grayscale <- data.frame(t(sapply(1:nrow(grayscale), function(x)
    sapply(1:ncol(grayscale), function(y)
      round(grayscale[x, y], 2)))))
  
  # Pseudo-melt
  grayscaleDF <- do.call(rbind, lapply(1:nrow(grayscale), function(x)
    do.call(rbind, lapply(1:ncol(grayscale), function(y)
      as.data.frame(t(as.matrix(c(grayscale[x, y], x, y))))
    ))))
  colnames(grayscaleDF) <- c("Value", "Row", "Column")
  
  # Only things with white-ish text
  reducedDF <- grayscaleDF %>% mutate(Value = ifelse(Value >= cutoffVal, 1, 0))
  reducedDF <- subset(reducedDF, Value >= cutoffVal)
  
  # There's at least a 1-pixel distance between letters?
  grayscale <- grayscale[rev(1:nrow(grayscale)), ]
  binaryGray <- data.frame(t(sapply(1:nrow(grayscale), function(x)
    sapply(1:ncol(grayscale), function(y)
      ifelse(grayscale[x, y] >= cutoffVal, 1, 0)))))
  
  uniqueVector <- sort(unique(reducedDF$Column))
  test <- sapply(1:(length(sort(unique(reducedDF$Column)))-1), function(x) {
                uniqueVector <- sort(unique(reducedDF$Column))
                ifelse(abs(uniqueVector[x] - uniqueVector[x+1]) != 1, 1, 0)
                })
  
  splitStarts <- c(uniqueVector[1], uniqueVector[match(uniqueVector[test == 1], uniqueVector) + 1])
  splitEnds <- c(uniqueVector[test == 1], uniqueVector[length(uniqueVector)])
  
  letterList <- lapply(1:length(splitStarts), function(x)
    binaryGray[, splitStarts[x]:splitEnds[x]])
  
  # Remove empty rows and add them to a list.  This removes spaces between dots and i/j
  trimmedLetterList <- list()
  trimmedLetterList <- sapply(1:length(letterList), function(x) {
    lappend(trimmedLetterList,
            as.matrix(do.call(rbind, lapply(1:nrow(letterList[[x]]),
                                            function(y)
                                              if (1 %in% letterList[[x]][y, ])
                                              {letterList[[x]][y, ] }
            )), byrow = TRUE)
    )
  })
  
  # Get dimensions of each one. Are these unique enough?
  sapply(trimmedLetterList, function(x) dim(x))
  
}

t1 <- binaryDims("Test.png", 0.98)
t1 <- data.frame(t(t1))
t1$Letter <- c(unlist(strsplit("EvilTiki", "")))

t2 <- binaryDims("Test2.png", 0.98)
t2 <- data.frame(t(t2))
t2$Letter <- c(unlist(strsplit("UndeadPaladin", "")))

# t3 <- binaryDims("Test3.png", 0.98)
# t3 <- data.frame(t(t3))
# t3$Letter <- c(unlist(strsplit("Diretooth", "")))
# 
# t4 <- binaryDims("Test4.png", 0.98)
# t4 <- data.frame(t(t4))
# t4$Letter <- c(unlist(strsplit("SupremeDarkshifter", "")))

test <- unique(rbind(t1, t2))
colnames(test) <- c("Cols", "Rows", "Letter")

