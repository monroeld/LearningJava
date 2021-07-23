library(plyr)
library(dplyr)
library(tidyverse)
library(padr)
library(readxl)

df <- read_xlsx("Barcode_MasterSheet.xlsx")

df <- df[complete.cases(df), ]

df <- df %>% mutate(SeqLen = nchar(Sequence))
