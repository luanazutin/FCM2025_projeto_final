df <- read.csv2("Base_dados_Cracidae.csv")

df$Average.Mass <- as.numeric(df$Average.Mass) #coluna de massa seja realmente numérica, não texto
#slide 1
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


#slide 2
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

####################################################################################

############################################################
# 0) CARREGAR DADOS E PREPARAR VARIÁVEIS
############################################################

# Pacotes que vamos usar
library(ggplot2)
library(dplyr)

# Ler a base (ajuste o caminho se precisar)
df <- read.csv2("Base_dados_Cracidae.csv",
                stringsAsFactors = FALSE)

# Garantir que as variáveis numéricas estão realmente como numéricas
df$Average.Mass      <- as.numeric(df$Average.Mass)
df$Elevational.Range <- as.numeric(df$Elevational.Range)
df$DB                <- as.numeric(df$DB)
df$ESI               <- as.numeric(df$ESI)

# Criar um data.frame só com as variáveis ecológicas numéricas
vars_ecologicas <- df %>%
  dplyr::select(Average.Mass, Elevational.Range, DB, ESI)

# Remover linhas com NA nessas variáveis para as análises multivariadas
vars_ecologicas_clean <- na.omit(vars_ecologicas)

# Só para conferir
summary(vars_ecologicas_clean)

############################################################
# CORRELAÇÕES ECOLÓGICAS
#(Massa, Altitude, DB, ESI)
############################################################

# Matriz de correlação (Spearman, que é mais robusta)
cor_spearman <- cor(vars_ecologicas_clean,
                    use = "pairwise.complete.obs",
                    method = "spearman")

cor_spearman  # isso imprime a matriz de correlação no console

######################## Gráfico 1: Massa corporal vs Amplitude altitudinal
ggplot(df, aes(x = Elevational.Range, y = Average.Mass)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Relação entre massa corporal e amplitude altitudinal (Cracidae)",
    x = "Amplitude altitudinal (m)",
    y = "Massa corporal média (g)"
  )

# Gráfico 2: Massa corporal vs ESI
ggplot(df, aes(x = ESI, y = Average.Mass)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Relação entre massa corporal e especialização ecológica (ESI)",
    x = "ESI",
    y = "Massa corporal média (g)"
  )

# Gráfico — Massa corporal vs Diversidade de Dieta (DB)
ggplot(df, aes(x = DB, y = Average.Mass)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Relação entre massa corporal e diversidade de dieta (DB)",
    x = "Diversidade de Dieta (DB)",
    y = "Massa corporal média (g)"
  )

# Correlação Spearman
cor.test(vars_ecologicas_clean$Average.Mass,
         vars_ecologicas_clean$DB,
         method = "spearman")



# correlação (Spearman):
cor.test(vars_ecologicas_clean$Average.Mass,
         vars_ecologicas_clean$Elevational.Range,
         method = "spearman")

cor.test(vars_ecologicas_clean$Average.Mass,
         vars_ecologicas_clean$ESI,
         method = "spearman")

install.packages("ggcorrplot")

library(ggcorrplot)

ggcorrplot(cor_spearman,
           method = "circle",
           lab = TRUE,
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_minimal(),
           title = "Matriz de correlação ecológica (Spearman)")



############################################################
# 3) K-MEANS + GRÁFICO DOS CLUSTERS NO ESPAÇO PCA
############################################################

set.seed(123)  # para resultados reproduzíveis

# K-means com 3 clusters (pode ajustar se quiser testar outros k)
k3 <- kmeans(vars_scaled, centers = 3, nstart = 25)

# Adicionar o cluster ao data.frame do PCA
pca_df$cluster <- factor(k3$cluster)

# Gráfico: clusters no espaço dos dois primeiros componentes principais
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Clusters ecológicos no espaço PCA (k-means, k = 3)",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  )



vars <- df %>%
  select(Latin, Average.Mass, Elevational.Range, DB, ESI) %>%
  drop_na()

vars$cluster <- k3$cluster
vars %>% 
  select(Latin, cluster) %>%
  arrange(cluster)






############################################################
# 4) COMPARAÇÃO ENTRE CLUSTERS
#    (VALIDAR SE OS GRUPOS SÃO DIFERENTES)
############################################################

# Juntar as variáveis ecológicas com o cluster
clusters_df <- as.data.frame(vars_ecologicas_clean)
clusters_df$cluster <- factor(k3$cluster)

# Ver um resumo rápido por cluster
clusters_df %>%
  group_by(cluster) %>%
  summarise(
    n          = n(),
    massa_med  = mean(Average.Mass, na.rm = TRUE),
    alt_med    = mean(Elevational.Range, na.rm = TRUE),
    DB_med     = mean(DB, na.rm = TRUE),
    ESI_med    = mean(ESI, na.rm = TRUE)
  )

# Boxplots por cluster – Massa
ggplot(clusters_df, aes(x = cluster, y = Average.Mass, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Massa corporal por cluster ecológico",
    x = "Cluster",
    y = "Massa corporal média (g)"
  ) +
  theme(legend.position = "none")

# Boxplots por cluster – Amplitude altitudinal
ggplot(clusters_df, aes(x = cluster, y = Elevational.Range, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Amplitude altitudinal por cluster ecológico",
    x = "Cluster",
    y = "Amplitude altitudinal (m)"
  ) +
  theme(legend.position = "none")

# Boxplots por cluster – ESI
ggplot(clusters_df, aes(x = cluster, y = ESI, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Especialização ecológica (ESI) por cluster",
    x = "Cluster",
    y = "ESI"
  ) +
  theme(legend.position = "none")

# Testes de Kruskal–Wallis entre clusters
kruskal.test(Average.Mass ~ cluster, data = clusters_df)
kruskal.test(Elevational.Range ~ cluster, data = clusters_df)
kruskal.test(DB ~ cluster, data = clusters_df)
kruskal.test(ESI ~ cluster, data = clusters_df)

