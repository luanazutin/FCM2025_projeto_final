#setwd("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/projeto_final")

library(openxlsx)
library(readxl)
library(tidyverse)

df <- read.csv2("Base_dados_Cracidae.csv")
names(df)
head(df)
colnames(df)

##brainstorm##
#criar pasta para salvar os gráficos
if (!dir.exists("graficos")) {
  dir.create("graficos")
}

# verificar quantidade de gêneros e espécies 
nrow(df) # 57 spp

contagem_gen <- df %>% 
  count(Genus) %>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))

#gráfico de pizza - atribuindo a varíavel
grafico_generos <- ggplot(contagem_gen, aes(x = "", y = n, fill = Genus)) +
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

ggsave("graficos/grafico_generos.png", plot = grafico_generos, width = 8, height = 6)

#Status de conservaçao - por genero? por distribuicao geografica? 

dados_ameacados <- df %>% 
  count(Genus, X2024.IUCN.Red.List.category) %>% 
  filter(X2024.IUCN.Red.List.category %in% c("VU", "EN", "CR"))

grafico_status_ameaca <- ggplot(dados_ameacados, aes(x = Genus, y = n, fill = X2024.IUCN.Red.List.category)) +
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

ggsave("graficos/grafico_status_ameaca.png", plot = grafico_status_ameaca, width = 8, height = 6)


#visão geral da massa corporal
df <- df %>% 
  mutate(Average.Mass = as.numeric(Average.Mass))

summary(df$Average.Mass)

#histograma
grafico_massa_hist <- ggplot(df, aes (x = Average.Mass))+
  geom_histogram(bins = 15)+
  theme_minimal()
ggsave("graficos/grafico_massa_hist.png", plot = grafico_massa_hist, width = 8, height = 6)

#boxplot
grafico_massa_box <- ggplot(df, aes(x = "", y = Average.Mass)) +
  geom_boxplot() +
  theme_classic()
ggsave("graficos/grafico_massa_box.png", plot = grafico_massa_box, width = 8, height = 6)

#média massa
media_massa_gen <- df %>%
  group_by(Genus) %>%
  summarise(media_massa = mean(Average.Mass, na.rm = TRUE))

grafico_media_massa_gen <- ggplot(media_massa_gen, aes(x = Genus, y = media_massa, fill = Genus)) +
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
ggsave("graficos/grafico_media_massa_gen.png", plot = grafico_media_massa_gen, width = 8, height = 6)**
  
  #library(RColorBrewer)
  #display.brewer.all()
  
  
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

#altitude média por gênero
media_alt_gen <- df %>%
  group_by(Genus) %>%
  summarise(media_altitude = mean(Elevational.Range, na.rm = TRUE))

grafico_altitude_gen <- ggplot(media_alt_gen, aes(y = media_altitude, x = Genus)) +  
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal()+
  theme(axis.text.x = element_text(face = "italic"))
ggsave("graficos/grafico_altitude_gen.png", plot = grafico_altitude_gen, width = 8, height = 6)

#histograma da altitude média por gênero
grafico_altitude_hist <- ggplot(media_alt_gen, aes(x = media_altitude)) +
  geom_histogram(bins = 15, fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Histograma: Altitude média por gênero",
    x = "Altitude média",
    y = "Contagem"
  )
ggsave("graficos/grafico_altitude_hist.png", plot = grafico_altitude_hist, width = 8, height = 6)

#boxplot
grafico_altitude_box <- ggplot(df, aes(x = Genus, y = Elevational.Range, fill = Genus)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired")+
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none")
ggsave("graficos/grafico_altitude_box.png", plot = grafico_altitude_box, width = 8, height = 6)

#Uso de habitat - gráfico de pizza
uso_habitat <- df %>%
  count(Primary.Habitat)%>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))

grafico_habitat <- ggplot(uso_habitat, aes(x = "", y = n, fill = Primary.Habitat)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
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

ggsave("graficos/grafico_habitat_pizza.png", plot = grafico_habitat, width = 8, height = 6)

#Dieta
dieta <- df %>%
  count(Primary.Diet)%>%
  mutate(porcentagem = round(n / sum(n) * 100, digits = 1))

#gráfico de pizza
grafico_dieta <- ggplot(dieta, aes(x = "", y = n, fill = Primary.Diet)) +
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

ggsave("graficos/grafico_dieta.png", plot = grafico_dieta, width = 8, height = 6)

#amplitude da dieta
#boxplot
grafico_dieta_boxplot <- ggplot(df, aes(x = Genus, y = DB, fill = Genus)) +
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
grafico_ESI_geral <- ggplot(especialidade, aes(x = "", y = ESI, fill = nível_especialidade)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    fill = "Nível de especialidade ecológica", x = "", y = ""
  ) 
ggsave("graficos/grafico_ESI_geral.png", plot = grafico_ESI_geral, width = 8, height = 6)

#por gênero
grafico_ESI_genero <-ggplot(especialidade, 
                            aes(x = Genus, fill = nível_especialidade)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  theme(axis.text.x = element_text(face = "italic", size = 12, angle = 45, hjust = 1)) +
  labs(
    fill = "Especialidade ecológica",
    x = "Gêneros",
    y = "ESI em proporção"
  )
ggsave("graficos/grafico_ESI_gen.png", plot = grafico_ESI_genero, width = 8, height = 6)

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


## Exportar tabelas resumo
write.xlsx(list(
  "Contagem por gênero" = contagem_gen,
  "Espécies ameaçadas" = dados_ameacados,
  "Massa média por gênero" = media_massa_gen,
  "Altitudes médias por gênero" = media_alt_gen,
  "Especialização ecológica" = especialidade
), file = "Resumo_Cracidae.xlsx")


