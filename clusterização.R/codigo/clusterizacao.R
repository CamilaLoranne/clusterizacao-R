# Instalar pacotes
install.packages("ggplot2", dependencies = TRUE)
install.packages("factoextra", dependencies = TRUE)

# Carregar bibliotecas
library(factoextra)
library(ggplot2)
library(readxl)

# Leitura do arquivo Excel
EXER_A3 <- read_excel("EXER_A3.xlsx")

# Padronização dos dados
df <- scale(EXER_A3)

# Reduzindo para variáveis selecionadas
df_reduced <- scale(EXER_A3[, c("salario", "posicao", "anosexperiencia")])

# Gráfico do cotovelo
fviz_nbclust(df_reduced, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# K-means
set.seed(123)
km.res = kmeans(df_reduced, 4, nstart = 25)
print(km.res)

# Adiciona cluster ao dataset original
EXER_A3_clustered <- cbind(EXER_A3, cluster = km.res$cluster)
head(EXER_A3_clustered)
km.res$centers

# Médias por cluster
aggregate(EXER_A3[, c("salario", "posicao", "anosexperiencia")],
          by = list(cluster = km.res$cluster),
          mean)

# Média salarial por sexo
aggregate(salario ~ sexo, data = EXER_A3, mean)

# Boxplot por sexo
ggplot(EXER_A3, aes(x = as.factor(sexo), y = salario)) +
  geom_boxplot(fill = "#00AFBB") +
  labs(x = "Sexo", y = "Salário", title = "Distribuição Salarial por Sexo") +
  theme_minimal()

# Visualização dos clusters
fviz_cluster(km.res, data = df_reduced,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

# Distância euclidiana
dista = dist(df, method = "euclidean")
as.matrix(dista)[1:3, 1:3]

# Clusterização hierárquica
dista.hc = hclust(d = dista, method = "ward.D")
fviz_dend(dista.hc, k = 4, rect = TRUE, cex = 0.5)

# Teste estatístico
t.test(salario ~ sexo, data = EXER_A3)

# Correlação salário x experiência
correlacao <- cor(EXER_A3$salario, EXER_A3$anosexperiencia)
print(paste("Correlação entre anos de experiência e salário:", round(correlacao, 2)))

# Gráfico de dispersão
ggplot(EXER_A3, aes(x = anosexperiencia, y = salario)) +
  geom_point(color = "00AFBB") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Anos de Experiência", y = "Salário", 
       title = "Relação entre Anos de Experiência e Salário") +
  theme_minimal()