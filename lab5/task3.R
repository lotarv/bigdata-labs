#Задание 3

library(factoextra)
library(cluster)
library(NbClust)
library(easystats)
library(mclust)


numeric_data <- na.omit(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")])
scaled_data <- scale(numeric_data)

# --- Метод локтя ---
print(
  fviz_nbclust(scaled_data, kmeans, method = "wss") +
    labs(title = "Метод локтя: определение оптимального числа кластеров")
)

# --- Метод силуэта ---
print(
  fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
    labs(title = "Метод силуэта: качество кластеризации")
)

# --- Статистика разрыва ---
set.seed(123)
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(
  fviz_gap_stat(gap_stat)
)

# --- Алгоритм консенсуса ---
set.seed(123)
n_clust <- n_clusters(numeric_data, package = c("easystats", "NbClust", "mclust"), standardize = TRUE)
print(plot(n_clust))
