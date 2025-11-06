setwd("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/PROJETO FINAL")

library(openxlsx)
library(readxl)

df <- read.xlsx("BIRDBASE.xlsx")
names(df)
head(df)