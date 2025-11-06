setwd("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/projeto_final")

library(openxlsx)
library(readxl)

df <- read.csv("Base_dados_Cracidae.csv")
names(df)
head(df)
colnames(df)
