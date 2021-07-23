# This is the user-interface definition of a Shiny web application. You can
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


# THIS IS WHERE SHINY UI STARTS

navbarPage("SD Chip Explorer",

           # This panel is for selecting run parameters and looking at the plots
           
           tabPanel("Parameters and Plots",
     
                    # Layout info available at:
                    # https://shiny.rstudio.com/articles/layout-guide.html 
                    
                    # This page setup uses percentages of the window's size to
                    #   scale the various columns and sub-columns.  The major
                    #   column widths need to add up to 12
                    fluidPage(
                      # There are two main columns:
                      #   Column 1: Contains subcolumns for parameters
                      #   Column 2: Contains subcolumns for graphs
                      fluidRow(
                        column(4, align = "center",
                              fluidRow(
                                column(6, align = "left", br(),
                                       "Options #1", br(), br(),
                                       
                                       # Dropdowns
                                       #   Long-term, it would be nice to list the number of
                                       #   arrays that fit each condition.  That's not super
                                       #   reasonable for this stage.
                                       selectInput('RefMat1', label = 'Reference Material',
                                                   choices = c(unique(Alldata$RefMat))),
                                       selectInput('WTfluor1', label = 'WT Probe Fluorophore',
                                                   choices = c(unique(Alldata$WTFluor))),
                                       selectInput('Triton1', label = 'Triton Concentration',
                                                   choices = c(All='.', sort(unique(Alldata$Triton)))),
                                       selectInput('Tween1', label = 'Tween Concentration',
                                                   choices = c(All='.', sort(unique(Alldata$Tween)))),
                                       selectInput('Master1', label = 'Master',
                                                   choices = c(All='.', sort(unique(Alldata$Master)))),
                                       selectInput('CellStain1', label = 'Cell Stain',
                                                   choices = c(All='.', unique(Alldata$CellStain))),
                                       selectInput('RunDate1', label = 'Run Date',
                                                   choices = c(All='.', sort(unique(Alldata$RunDate)))),
                                       # selectInput('ArrayID1', label = 'Array ID',
                                       #             choices = c(All = '.', unique(Alldata$ArrayID))),
                                       br(), textInput('RowSubset1',
                                                 label = "Rows to Remove", value = ""),
                                       textInput('ColumnSubset1',
                                                 label = "Columns to Remove", value = "")
                                       ),
                                
                                column(6, align = "left", br(),
                                       "Options #2", br(), br(),
                                       
                                       # Dropdowns
                                       #   Long-term, it would be nice to list the number of
                                       #   arrays that fit each condition.  That's not super
                                       #   reasonable for this stage.
                                       selectInput('RefMat2', label = 'Reference Material',
                                                   choices = c(unique(Alldata$RefMat))),
                                       selectInput('WTfluor2', label = 'WT Probe Fluorophore',
                                                   choices = c(unique(Alldata$WTFluor))),
                                       selectInput('Triton2', label = 'Triton Concentration',
                                                   choices = c(All='.', sort(unique(Alldata$Triton)))),
                                       selectInput('Tween2', label = 'Tween Concentration',
                                                   choices = c(All='.', sort(unique(Alldata$Tween)))),
                                       selectInput('Master2', label = 'Master',
                                                   choices = c(All='.', sort(unique(Alldata$Master)))),
                                       selectInput('CellStain2', label = 'Cell Stain',
                                                   choices = c(All='.',unique(Alldata$CellStain))),
                                       selectInput('RunDate2', label = 'Run Date',
                                                   choices = c(All='.', sort(unique(Alldata$RunDate)))),
                                       br(), textInput('RowSubset2',
                                                 label = "Rows to Remove", value = ""),
                                       textInput('ColumnSubset2',
                                                 label = "Columns to Remove", value = "")
                                       # selectInput('ArrayID2', label = 'Array ID',
                                       #             choices = c(All = '.', unique(Alldata$ArrayID))),
                                       )
                              ),
                              
                              "(For row and column subsetting, separate values
                              with a comma and denote sequences with a colon)",
                              br(), br(),
                              
                              # Submit button
                              submitButton("Submit")
                        ),
                        column(8, align = "center",
                               fluidRow(
                                 column(6, align = "center", br(),
                                        "Plots #1", br(), br(),
                                        plotOutput("zygBarPlot1"), br(),
                                        verbatimTextOutput("zygArrays1"), br(),
                                        # tableOutput("errorDF1"),
                                        plotOutput("errorBarPlot1")
                                        # plotOutput("pieChart1", height = "250px")
                                  ),
                                 column(6, align = "center", br(),
                                        "Plots #2", br(), br(),
                                        plotOutput("zygBarPlot2"), br(),
                                        verbatimTextOutput("zygArrays2"), br(),
                                        # tableOutput("errorDF2"),
                                        plotOutput("errorBarPlot2")
                                        # plotOutput("pieChart2", height = "250px")
                                  )
                               )
                        )
                      )
                      
                    )  # End of fluidPage
            ),  # End of tabPanel "Parameters and Plots"
           
           tabPanel("Statistical Analysis",
                    "Nothing to see here (yet), move along"
            )
           
) # End of navbarPage

