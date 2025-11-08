#setwd("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/projeto_final")

library(openxlsx)
library(readxl)
library(tidyverse)

df <- read.csv2("Base_dados_Cracidae.csv")
names(df)
head(df)
colnames(df)

##brainstorm##
# verificar quantidade de gêneros e espécies 
nrow(df) # 57 spp
contagem_gen <- df %>% 
  count(Genus) %>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))

ggplot(contagem_gen, aes(x = "", y = n, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  theme_minimal()+
  geom_text(
    aes(label = porcentagem), 
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribuição de Espécies por Gênero (Cracidae)",
    fill = "Gêneros" 
  )
#Status de conservaçao - por genero? por distribuicao geografica? 
#visão geral da massa corporal
#visão geral sobre a alimentacao e uso de habitat?

##informacao das legendas##
#RR = Restricted range (global range size<50,000 km2 ) species get a 1
#ISL = 1 = Species' breeding is restricted to island(s), 0 = species not restricted to an island
#RLM = Realm; A	Australian;C	Cosmopolitan;E	Eastern Hemisphere;
# F	Afrotropical;I	Indomalayan;L	Neotropical;M	Madagascar & islands;N	Nearctic;
# O	Oceania;P	Palearctic;S	South Polar;
# W	Wallacea (see: https://unair.ac.id/pre-historic-wallacea-a-melting-pot-of-human-genetic-ancestries/); Z	New Zealand & islands
#LAT = Latitudinal range of species based on tropics and polar circles.
#HB = Habitat breadth, the number of major habitats used
#DB = Diet breadth, the number of major food types consumed
#ESI = Ecological Specialization Index: log10 (100/[dietary breadth x habitat breadth]), with a maximum of 2 for the most specialized species that only feed on one major food group and live in one major type of habitat (e.g., forest frugivore; Sekercioglu, 2011)




