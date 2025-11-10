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
#gráfico de pizza
ggplot(contagem_gen, aes(x = "", y = n, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  theme_void()+
  # geom_text(
  #   aes(label = porcentagem), 
  #   position = position_stack(vjust = 0.5)
  # ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribuição de Espécies por Gênero (Cracidae)",
    fill = "Gêneros", x = "", y = ""
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(face = "italic", size = 12)
  )

ggsave("primeiro_grafico.png")

#Status de conservaçao - por genero? por distribuicao geografica? 

dados_ameacados <- df %>% 
  count(Genus, X2024.IUCN.Red.List.category) %>% 
  filter(X2024.IUCN.Red.List.category %in% c("VU", "EN", "CR"))

ggplot(dados_ameacados, aes(x = Genus, y = n, fill = X2024.IUCN.Red.List.category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "VU" = "gold",
    "EN" = "darkorange",
    "CR" = "firebrick" 
  )) +
  labs(
    title = "Status de ameaça por Gênero",
    x = "Gêneros",
    y = "total", 
    fill = "Status de ameaça IUCN"
  )


#visão geral da massa corporal
df <- df %>% 
  mutate(Average.Mass = as.numeric(Average.Mass))

summary(df$Average.Mass)

#histograma
ggplot(df, aes (x = Average.Mass))+
  geom_histogram(bins = 15)+
  theme_minimal()

#boxplot
ggplot(df, aes(x = "", y = Average.Mass)) +
  geom_boxplot() +
  theme_classic()

media_massa_gen <- df %>%
  group_by(Genus) %>%
  summarise(media_massa = mean(Average.Mass, na.rm = TRUE))

ggplot(media_massa_gen, aes(x = Genus, y = media_massa, fill = Genus)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired")+
  coord_flip() +
    labs(
    title = "Média da massa por gêneros",
    x = "Gêneros",
    y = "Média da massa corporal"
  ) +
  theme_classic() +
  theme(
    legend.title = element_text(size = 12),
    axis.text.y = element_text(face = "italic", size = 12),
    legend.position = "none"
  )
# library(RColorBrewer)
# display.brewer.all()


#visão geral sobre ocorrencias
#contagem de Restricted range
df %>% 
  count(RR)

#contagem de species' breeding is restricted to island(s) (seria endêmicas)
df %>% 
  count(ISL)

df %>%
  filter(ISL == T) #verifiquei que é a espécie Pipile pipile

df %>% 
  count(RLM) #todos são neotropical

df %>%
  count(LAT)

#amplitude altitudional
df %>%
  count(Elevational.Range)

str(df)

media_alt_gen <- df %>%
  group_by(Genus) %>%
  summarise(media_altitude = mean(Elevational.Range, na.rm = TRUE))

ggplot(media_alt_gen, aes(y = media_altitude, x = Genus)) +  
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  theme(axis.text.x = element_text(face = "italic"))
  
ggplot(media_alt_gen, aes(x = media_altitude)) +
  geom_histogram(bins = 15, fill = "steelblue")

#boxplot
ggplot(df, aes(x = Genus, y = Elevational.Range, fill = Genus)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired")+
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none")

#Uso de habitat
df %>%
  count(Primary.Habitat)

uso_habitat <- df %>% 
  count(Primary.Habitat) %>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))


#gráfico de pizza
ggplot(uso_habitat, aes(x = "", y = n, fill = Primary.Habitat)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Forest" = "Floresta",
                      "Shrub" = "Arbusto",
                      "Woodland" = "Bosque"))+
  scale_fill_manual(values = c(
    "Forest" = "green",
    "Shrub" = "yellow",
    "Woodland" = "green4"
  ))+
  theme_void()+
  labs(
    title = "Uso do Habitat (Cracidae)",
    fill = "Habitat", x = "", y = ""
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

#Dieta
df %>%
  count(Primary.Diet)

dieta <- df %>% 
  count(Primary.Diet) %>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))


#gráfico de pizza
ggplot(dieta, aes(x = "", y = n, fill = Primary.Diet)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_discrete(labels = c("Fruit" = "Fruta",
                                 "Herbivore" = "Herbívoro",
                                 "Omnivore" = "Onívoro",
                                 "Plant" = "Plantas"))+
  scale_fill_manual(values = c(
    "Fruit" = "red3",
    "Herbivore" = "green3",
    "Omnivore" = "orange3",
    "Plant" = "green"
  ))+
  theme_void()+
  labs(
    title = "Dieta (Cracidae)",
    fill = "Dieta", x = "", y = ""
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

#amplitude da dieta
#boxplot
ggplot(df, aes(x = Genus, y = DB, fill = Genus)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired")+
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none")

#ESI
especialidade <- df %>%
  select(Latin, Genus, ESI) %>%
  mutate(ESI = as.numeric(ESI)) %>%
  mutate(nível_especialidade = case_when(
    ESI < 1 ~ "Generalista",
    ESI == 1 ~ "Médio especialista",
    TRUE ~ "Especialistas"
  ))
#geral
ggplot(especialidade, aes(x = "", y = ESI, fill = nível_especialidade)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    fill = "Nível de especialidade ecológica", x = "", y = ""
  ) 
  
#por gênero
ggplot(especialidade, 
       aes(x = Genus, fill = nível_especialidade)) +
   geom_bar(position = "fill") + 
  theme_minimal() +
  theme(axis.text.x = element_text(face = "italic", size = 12, angle = 45, hjust = 1)) +
  labs(
    fill = "Especialidade ecológica",
    x = "Gêneros",
    y = "ESI em proporção"
  )



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




