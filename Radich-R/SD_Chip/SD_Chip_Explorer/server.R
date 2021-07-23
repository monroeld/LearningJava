# This is the server definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Initialize libraries

require(shiny)
require(ggplot2)
require(ggrepel)
require(ggExtra)
require(plyr)
require(dplyr)
require(tidyr)
require(gridExtra)
require(reshape2)
require(RColorBrewer)
require(rstudioapi)
require(data.table)


histalpha <- 0.45
scatteralpha <- histalpha * 2/3
fluorlist <- c("FAM", "HEX", "Cy5")
  
welltotal <- 1024

basepath <- getwd()
  
serverinput = basepath
serveroutput = basepath
  
lappend <- function(lst, obj) {
  lst[[length(lst)+1]] <- obj
  return(lst)
}


Alldata <<- read.csv(file = "Compiled_Shiny_Data.csv", header = TRUE, sep = ",",
                     stringsAsFactors = FALSE)
Alldata$Zygosity <<- factor(Alldata$Zygosity, levels = c("WT", "HET", "MUT", NA))


texttonumeric <- function(stringinput) {
  
  # Remove spaces
  if (grepl(" ", stringinput)) {
    stringinput <- gsub(" ", "", stringinput)
  }
  
  # If there's a comma, split around it.
  #   Make each element a new object in a vector
  if (grepl(",", stringinput)) {
    stringinput <- c(sapply(stringinput, function(x)
                            if (grepl(",", x)) {
                                c(unlist(strsplit(x, ",")))
                            }
                    ))
  }

  
  # If there's a colon in any element of nocommas, it creates a sequence
  #   of numbers from the pre-colon number to the post-colon number
  stringinput <- c(unlist(lapply(stringinput, function(x)
    if (grepl(":", x)) {
      storage <- c(seq(as.numeric(c(unlist(strsplit(x, ":"))))[1], 
                       as.numeric(c(unlist(strsplit(x, ":"))))[2], 1))
    } else {
      storage <- c(as.numeric(x))
    }
  )))
  
  return(stringinput)
}

zygplot <- function(dataframe) {
  zyg <- ggplot(dataframe,
                 aes_string(x = 'Zygosity', fill = 'Zygosity')) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(stat="count", aes(y = (..count..)/sum(..count..),
                                label = ..count..), vjust = -0.5, size = 5) +
    labs(x = "Zygosity", y = "Fraction") + scale_fill_manual(values = zygcolors) +
    theme(legend.position = "none", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11))
  
  return(zyg)
}

buildpiearray <- function(dataframe) {
  dataframe$Usable <- 0
  dataframe <- mutate(dataframe, Usable=replace(Usable, !is.na(FAMAID),"Usable"))
  dataframe <- mutate(dataframe, Usable=replace(Usable, is.na(FAMAID),"Unusable"))
  
  ampfluorcol <- grep(paste0(dataframe$AmpFluor[1], "AID"), colnames(dataframe))
  amplimscol <- grep(paste0(dataframe$AmpFluor[1], "lims"), colnames(dataframe))
  wtfluorcol <- grep(paste0(dataframe$WTFluor[1], "AID"), colnames(dataframe))
  wtlimscol <- grep(paste0(dataframe$WTFluor[1], "lims"), colnames(dataframe))
  mutfluorcol <- grep(paste0(dataframe$MUTFluor[1], "AID"), colnames(dataframe))
  mutlimscol <- grep(paste0(dataframe$MUTFluor[1], "lims"), colnames(dataframe))
  welltotal <- nrow(dataframe)
  nEmpty <- round(sum(dataframe$Usable=="Unusable")*100/welltotal, 2)
  
  # Cell data calls
  n0Cells <- round(sum(dataframe$Usable == "Usable" &
                         dataframe$CellGroup == 0)*100/welltotal, 2)
  n1Cell <- round(sum(dataframe$Usable == "Usable" &
                        dataframe$CellGroup == 1)*100/welltotal, 2)
  nxCells <- round(sum(dataframe$Usable == "Usable" &
                         dataframe$CellGroup == "2 or more")*100/welltotal, 2)
  
  # Plasmid data calls
  nAmpNeg <- round(sum(dataframe$Usable=="Usable" &
                         dataframe$CellGroup=="N/A" &
                         dataframe[, ampfluorcol] < dataframe[, amplimscol],
                       na.rm = T)*100/welltotal, 2)
  nWT <- round(sum(dataframe$Zygosity == "WT" &
                     dataframe$CellGroup=="N/A",
                   na.rm = T)*100/welltotal, 2)
  nMUT <- round(sum(dataframe$Zygosity == "MUT" &
                      dataframe$CellGroup=="N/A",
                    na.rm = T)*100/welltotal, 2)
  nHET <- round(sum(dataframe$Zygosity == "HET" &
                      dataframe$CellGroup=="N/A",
                    na.rm = T)*100/welltotal, 2)
  noZyg <- round(sum(dataframe$Usable == "Usable" &
                       dataframe[, ampfluorcol] >= dataframe[, amplimscol] &
                       is.na(dataframe$Zygosity))*100/welltotal, 2)
  
  if (n0Cells != 0 | n1Cell != 0 | nxCells != 0) {
    piearray <- data.frame(nEmpty, n0Cells, n1Cell, nxCells)
    colnames(piearray) <- c("% Empty", "% 0 Cells", "% 1 Cell", "% >1 Cell")
  } else {
    piearray <- data.frame(nEmpty, noZyg, nAmpNeg, nWT, nMUT, nHET)
    colnames(piearray) <- c("% Empty", "% no Zyg", "% AmpNeg", "% WT", "% MUT", "% HET")
  }
  return(piearray)
}

pieplot <- function(dataframe) {
  pie <- ggplot(dataframe,
                 aes_string(x = factor(1), y = "value", fill = "variable")) +
    geom_bar(stat = 'identity', width = 1) +
    coord_polar(theta = "y", direction = 1) +
    labs(fill = filllabel) + scale_fill_manual(values = piecolors) +
    geom_text_repel(aes_string(x = 1.7, y = "yposition", label = "value"),
                    box.padding = unit(0.05, "lines"), size = 3.75) +
    theme(axis.title=element_blank(), axis.text=element_blank(),
          axis.ticks=element_blank(), panel.grid=element_blank(),
          panel.spacing = unit(8.25, "lines"), legend.position = "left",
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 14))
  
  return(pie)
}

errorplot <- function(dataframe) {
  barplot <- ggplot(dataframe,
                    aes_string(x = factor(1), y = "percent", fill = "variable",
                               order = "variable")) +
    geom_bar(stat = "identity") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "right", legend.key.height = unit(3, "line"),
          legend.text = element_text(size = 12), plot.title = element_text(size = 16),
          panel.background = element_blank()) +
    scale_fill_manual(values = brewer.pal(n = 9, "Set1")[1:5],
                        labels = c("True\nPositive", "False\nPositive",
                                 "False\nNegative", "True\nNegative", "Other")) +
    guides(fill = guide_legend(reverse = FALSE)) +
    geom_text(aes_string(x = 1, y = "ypos", label = "percent"), size = 5) +
    labs(fill = "") +
    ggtitle("Percent of wells by cell number\nand amplification status")

  return(barplot)
}


set1 <- brewer.pal(n = 9, "Set1")
set3 <- brewer.pal(n = 12, "Set3")
zygcolors <- c(set3[5], set3[10], set3[4])

cellpiecolors <- c(set1[7], set1[5], set1[3], set1[6])
plasmidpiecolors <- c(set1[7], set1[9], set1[8],
                      zygcolors[1], zygcolors[3], zygcolors[2])



shinyServer(function(input, output, session) {
  


  
  # Define plotdata1 and 2, which are lists.  For each...
  #  Arg 1:  dataframe to pass to zygplot
  #  Arg 2:  number of arrays to pass to numArrays
  plotdata1 <<- reactive({
    
    # plotdata will always have some value for RefMat and WTFluor
    plotdataA <<- Alldata[Alldata$RefMat == input$RefMat1 &
                            Alldata$WTFluor == input$WTfluor1 &
                            !is.na(Alldata$Zygosity), ]
    
    if (input$RowSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Row %in% texttonumeric(input$RowSubset1)))
    }
    if (input$ColumnSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Column %in% texttonumeric(input$ColumnSubset1)))
    }
    
    # For the rest of these, subset if the user hasn't selected "All"
    if (input$Triton1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Triton == input$Triton1, ]
    }
    if (input$Tween1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Tween == input$Tween1, ]
    }
    if (input$Master1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Master == input$Master1, ]
    }
    if (input$CellStain1 != ".") {
      plotdataA <<- plotdataA[plotdataA$CellStain == input$CellStain1, ]
    }
    if (input$RunDate1 != ".") {
      plotdataA <<- plotdataA[plotdataA$RunDate == input$RunDate1, ]
    }
    
    nArrays1 <- (length(unique(plotdataA$ArrayID)))
    output1 <- list(plotdataA, nArrays1)
    
    return(output1)
  })
  
  plotdata2 <<- reactive({
    
    # plotdata will always have some value for RefMat and WTFluor
    plotdataB <<- Alldata[Alldata$RefMat == input$RefMat2 &
                            Alldata$WTFluor == input$WTfluor2 &
                            !is.na(Alldata$Zygosity), ]
    
    if (input$RowSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Row %in% texttonumeric(input$RowSubset2)))
    }
    if (input$ColumnSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Column %in% texttonumeric(input$ColumnSubset2)))
    }
    
    # For the rest of these, subset if the user hasn't selected "All"
    if (input$Triton2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Triton == input$Triton2, ]
    }
    if (input$Tween2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Tween == input$Tween2, ]
    }
    if (input$Master2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Master == input$Master2, ]
    }
    if (input$CellStain2 != ".") {
      plotdataB <<- plotdataB[plotdataB$CellStain == input$CellStain2, ]
    }
    if (input$RunDate2 != ".") {
      plotdataB <<- plotdataB[plotdataB$RunDate == input$RunDate2, ]
    }
    
    nArrays2 <- (length(unique(plotdataB$ArrayID)))
    output2 <- list(plotdataB, nArrays2)
    
    return(output2)
  })
  

  # Create zygosity barplots using zygplot function
  output$zygBarPlot1 <- renderPlot({ zygplot(plotdata1()[[1]]) })
  output$zygBarPlot2 <- renderPlot({ zygplot(plotdata2()[[1]]) })

  
  # Report number of arrays
  output$zygArrays1 <- renderText({
    paste("Arrays with these parameters: ", plotdata1()[[2]])
  })
  output$zygArrays2 <- renderText({
    paste("Arrays with these parameters: ", plotdata2()[[2]])
  })
  
  

  # Define data sets for pie charts.
  #   The subsetting is almost the same as above (is.na(Alldata$Zygosity) is removed),
  #   but these reactive also call the buildpiearray function.
  piedf1 <<- reactive({
    # plotdata will always have some value for RefMat and WTFluor
    plotdataA <<- Alldata[Alldata$RefMat == input$RefMat1 &
                            Alldata$WTFluor == input$WTfluor1, ]
    
    if (input$RowSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Row %in% texttonumeric(input$RowSubset1)))
    }
    if (input$ColumnSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Column %in% texttonumeric(input$ColumnSubset1)))
    }
    
    # For the rest of these, subset if the user hasn't selected "All"
    if (input$Triton1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Triton == input$Triton1, ]
    }
    if (input$Tween1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Tween == input$Tween1, ]
    }
    if (input$Master1 != ".") {
      plotdataA <<- plotdataA[plotdataA$Master == input$Master1, ]
    }
    if (input$CellStain1 != ".") {
      plotdataA <<- plotdataA[plotdataA$CellStain == input$CellStain1, ]
    }
    if (input$RunDate1 != ".") {
      plotdataA <<- plotdataA[plotdataA$RunDate == input$RunDate1, ]
    }
    
    piedf <<- buildpiearray(plotdataA)
    piedf <<- melt(piedf)
    piedf <<- mutate(piedf, yposition = 100-cumsum(value) + value/2)
    piedf <<- mutate(piedf, variable = as.factor(variable))
    
    if ("% HET" %in% piedf$variable) {
      piecolors <<- plasmidpiecolors
      filllabel <<- "Zygosity\nBreakdown"
    } else {
      piecolors <<- cellpiecolors
      filllabel <<- "Cell Counts"
    }
    
    return(piedf)
  })
  
  piedf2 <<- reactive({
    # plotdata will always have some value for RefMat and WTFluor
    plotdataB <<- Alldata[Alldata$RefMat == input$RefMat2 &
                            Alldata$WTFluor == input$WTfluor2, ]
    
    if (input$RowSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Row %in% texttonumeric(input$RowSubset2)))
    }
    if (input$ColumnSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Column %in% texttonumeric(input$ColumnSubset2)))
    }
    
    # For the rest of these, subset if the user hasn't selected "All"
    if (input$Triton2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Triton == input$Triton2, ]
    }
    if (input$Tween2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Tween == input$Tween2, ]
    }
    if (input$Master2 != ".") {
      plotdataB <<- plotdataB[plotdataB$Master == input$Master2, ]
    }
    if (input$CellStain2 != ".") {
      plotdataB <<- plotdataB[plotdataB$CellStain == input$CellStain2, ]
    }
    if (input$RunDate2 != ".") {
      plotdataB <<- plotdataB[plotdataB$RunDate == input$RunDate2, ]
    }
    
    piedf <<- buildpiearray(plotdataB)
    piedf <<- melt(piedf)
    piedf <<- mutate(piedf, yposition = 100-cumsum(value) + value/2)
    piedf <<- mutate(piedf, variable = as.factor(variable))
    
    if ("% HET" %in% piedf$variable) {
      piecolors <<- plasmidpiecolors
      filllabel <<- "Zygosity\nBreakdown"
    } else {
      piecolors <<- cellpiecolors
      filllabel <<- "Cell Counts"
    }
    
    return(piedf)
  })
  
  # Plot the pie charts using pieplot function
  output$pieChart1 <- renderPlot({ pieplot(piedf1()) })
  output$pieChart2 <- renderPlot({ pieplot(piedf2()) })
  
  
  
  # Define data set for error bar plot
  errorBarPlot1 <<- reactive({
    plotdataA <<- Alldata[Alldata$RefMat == input$RefMat1 &
                            Alldata$WTFluor == input$WTfluor1, ]
    
    if (input$RowSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Row %in% texttonumeric(input$RowSubset1)))
    }
    if (input$ColumnSubset1 != "") {
      plotdataA <<- subset(plotdataA, !(Column %in% texttonumeric(input$ColumnSubset1)))
    }
    
    if (grepl("Cells",plotdataA$RefMat[1])) {
      # For the rest of these, subset if the user hasn't selected "All"
      if (input$Triton1 != ".") {
        plotdataA <<- plotdataA[plotdataA$Triton == input$Triton1, ]
      }
      if (input$Tween1 != ".") {
        plotdataA <<- plotdataA[plotdataA$Tween == input$Tween1, ]
      }
      if (input$Master1 != ".") {
        plotdataA <<- plotdataA[plotdataA$Master == input$Master1, ]
      }
      if (input$CellStain1 != ".") {
        plotdataA <<- plotdataA[plotdataA$CellStain == input$CellStain1, ]
      }
      if (input$RunDate1 != ".") {
        plotdataA <<- plotdataA[plotdataA$RunDate == input$RunDate1, ]
      }
      
      TP <- sum(plotdataA$FAMAID >= plotdataA$FAMlims & plotdataA$CellGroup == 1, na.rm = TRUE)
      FP <- sum(plotdataA$FAMAID >= plotdataA$FAMlims & plotdataA$CellGroup == 0, na.rm = TRUE)
      FN <- sum(plotdataA$FAMAID < plotdataA$FAMlims & plotdataA$CellGroup == 1, na.rm = TRUE)
      TN <- sum(plotdataA$FAMAID < plotdataA$FAMlims & plotdataA$CellGroup == 0, na.rm = TRUE)
      Other <- nrow(plotdataA) - (TP + FP + FN + TN)
      
      errordfA <- data.frame(c(TP, FP, FN, TN, Other))
      errordfA$variable <- c("True Positive", "False Positive", "False Negative",
                             "True Negative", "Other")
      colnames(errordfA) <- c("value", "variable")
      errordfA$percent <- round(errordfA$value/sum(errordfA$value)*100, 2)
      errordfA <- errordfA[, c(2, 1, 3)]
      
      errordfA$variable <- factor(errordfA$variable,
                                  levels = c("True Positive", "False Positive",
                                                 "False Negative", "True Negative", "Other"))
      errordfA <- mutate(errordfA, ypos = round(100-cumsum(percent) + percent/2, 2))
      
      output <- errorplot(errordfA)
    } else {
      errordfA <- matrix(c("Sorry, error rates", "can only be calculated", "for cell data"))
      errordfA <- as.data.frame(errordfA)
      colnames(errordfA) <- NULL
      output <- grid.table(errordfA, theme = ttheme_minimal(base_size = 20), rows = NULL)
    }
    return(output)
  })
  
  errorBarPlot2 <<- reactive({
    plotdataB <<- Alldata[Alldata$RefMat == input$RefMat2 &
                            Alldata$WTFluor == input$WTfluor2, ]
    
    if (input$RowSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Row %in% texttonumeric(input$RowSubset2)))
    }
    if (input$ColumnSubset2 != "") {
      plotdataB <<- subset(plotdataB, !(Column %in% texttonumeric(input$ColumnSubset2)))
    }
    
    if (grepl("Cells",plotdataB$RefMat[1])) {
      # For the rest of these, subset if the user hasn't selected "All"
      if (input$Triton2 != ".") {
        plotdataB <<- plotdataB[plotdataB$Triton == input$Triton2, ]
      }
      if (input$Tween2 != ".") {
        plotdataB <<- plotdataB[plotdataB$Tween == input$Tween2, ]
      }
      if (input$Master2 != ".") {
        plotdataB <<- plotdataB[plotdataB$Master == input$Master2, ]
      }
      if (input$CellStain2 != ".") {
        plotdataB <<- plotdataB[plotdataB$CellStain == input$CellStain2, ]
      }
      if (input$RunDate2 != ".") {
        plotdataB <<- plotdataB[plotdataB$RunDate == input$RunDate2, ]
      }
      
      TP <- sum(plotdataB$FAMAID >= plotdataB$FAMlims & plotdataB$CellGroup == 1, na.rm = TRUE)
      FP <- sum(plotdataB$FAMAID >= plotdataB$FAMlims & plotdataB$CellGroup == 0, na.rm = TRUE)
      FN <- sum(plotdataB$FAMAID < plotdataB$FAMlims & plotdataB$CellGroup == 1, na.rm = TRUE)
      TN <- sum(plotdataB$FAMAID < plotdataB$FAMlims & plotdataB$CellGroup == 0, na.rm = TRUE)
      Other <- nrow(plotdataB) - (TP + FP + FN + TN)
      
      errordfB <- data.frame(c(TP, FP, FN, TN, Other))
      errordfB$variable <- c("True Positive", "False Positive", "False Negative",
                             "True Negative", "Other")
      colnames(errordfB) <- c("value", "variable")
      errordfB$percent <- round(errordfB$value/sum(errordfB$value)*100, 2)
      errordfB <- errordfB[, c(2, 1, 3)]
      
      errordfB$variable <- factor(errordfB$variable,
                                  levels = c("True Positive", "False Positive",
                                                 "False Negative", "True Negative", "Other"))
      errordfB <- mutate(errordfB, ypos = round(100-cumsum(percent) + percent/2, 2))
      
      output <- errorplot(errordfB)
    } else {
      errordfB <- matrix(c("Sorry, error rates", "can only be calculated", "for cell data"))
      errordfB <- as.data.frame(errordfB)
      colnames(errordfB) <- NULL
      output <- grid.table(errordfB, theme = ttheme_minimal(base_size = 20), rows = NULL)
    }
    return(output)
  })
  
  # Plot either the error barplot or the "Nope" tableGrob
  output$errorBarPlot1 <- renderPlot({ errorBarPlot1() })
  output$errorBarPlot2 <- renderPlot({ errorBarPlot2() })
  
  
  
  # The stuff below here is text that I'd like to be able to customize - namely,
  #   titles and column headers.  Outputs are the easiest way to do that
  
  output$parameters1 <- renderText({ "Set #1 Parameters "})
  output$parameters2 <- renderText({ "Set #1 Parameters "})
  
  output$plots1 <- renderText({ "Set #1 Plots" })
  output$plots2 <- renderText({ "Set #2 Plots" })
  
  
  
})