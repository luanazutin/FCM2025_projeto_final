df <- read.csv2("Base_dados_Cracidae.csv")

df$Average.Mass <- as.numeric(df$Average.Mass) #coluna de massa seja realmente numérica, não texto

summary(df$Average.Mass) #Mostra: valor mínimo, 1º quartil, mediana, média, 3º quartil, valor máximo
library(ggplot2)

ggplot(df, aes(x = Average.Mass)) +
  geom_histogram(
    bins = 12,
    fill = "#69b3a2",
    color = "white",
    alpha = 0.9,
    size = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(0, 4000, by = 500),
    limits = c(0, 4000)
  ) +
  theme_minimal() +
  labs(
    title = "Histograma da massa corporal média (Cracidae)",
    x = "Massa corporal média (g)",
    y = "Frequência"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) #histograma da massa corporal



### 1 - Teste Kruskal–Wallis

kruskal.test(Average.Mass ~ Genus, data = df) #esse teste verifica se a massa corporal difere estatisticamente entre os gêneros
#Como interpretar o resultado:
  #Se o p-valor < 0.05 → existe diferença significativa na massa corporal entre pelo menos dois gêneros.
  #Se p-valor ≥ 0.05 → não há diferença estatística.

pairwise.wilcox.test(df$Average.Mass, df$Genus,
                     p.adjust.method = "bonferroni") #Comparar quais gêneros são diferentes entre si


library(ggplot2)

ggplot(df, aes(x = Genus, y = Average.Mass, fill = Genus)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    legend.position = "none"
  ) +
  labs(
    title = "Distribuição da massa corporal por gênero (Cracidae)",
    x = "Gênero",
    y = "Massa corporal média (g)"
  ) #boxplot da massa por gênero

### 2 - Correlação massa × amplitude altitudinal
df$Elevational.Range <- as.numeric(df$Elevational.Range)
summary(df$Elevational.Range)

library(tidyverse)
dados_cor <- df %>% 
  dplyr::select(Average.Mass, Elevational.Range) %>% 
  tidyr::drop_na()

### Teste de correlação
cor.test(dados_cor$Average.Mass,
         dados_cor$Elevational.Range,
         method = "spearman")

library(ggplot2)

ggplot(dados_cor, aes(x = Average.Mass, y = Elevational.Range)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Relação entre massa corporal e amplitude altitudinal (Cracidae)",
    x = "Massa corporal média (g)",
    y = "Amplitude altitudinal (m)"
  )


### 4 - Clustering ecológico (k-means)
df <- read.csv2("Base_dados_Cracidae.csv")

df$Average.Mass      <- as.numeric(df$Average.Mass)
df$Elevational.Range <- as.numeric(df$Elevational.Range)
df$DB                <- as.numeric(df$DB)
df$ESI               <- as.numeric(df$ESI)

library(dplyr)

vars <- df %>%
  select(Average.Mass, Elevational.Range, DB, ESI) %>%
  drop_na()

vars_scaled <- scale(vars)  # transforma todas com média 0 e desvio 1

set.seed(1)  # para resultados reproduzíveis

clust <- kmeans(vars_scaled, centers = 3, nstart = 25)
clust


#Visualizar os clusters em gráfico
library(factoextra)

fviz_cluster(clust,
             data = vars_scaled,
             geom = "point",
             main = "Clusters ecológicos (k-means) – Cracidae")

#Ver a qual cluster cada espécie pertence
# criar um vetor de clusters do mesmo tamanho que vars
clusters <- clust$cluster

# juntar ao data frame filtrado (sem NA)
vars_com_cluster <- cbind(vars, cluster = clusters)

head(vars_com_cluster)

df_sem_na <- df %>%
  select(Latin, Average.Mass, Elevational.Range, DB, ESI) %>%
  drop_na()

df_sem_na$cluster <- clust$cluster

head(df_sem_na)


