library(factoextra)
library(cluster)
library(parameters)
library(NbClust)
library(mclust)
library(easystats)
library(e1071)
library(party)
library(randomForest)

# Чтение файла
data <- read.csv("countries.csv", 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "-9 999", 
                 fileEncoding = "Windows-1251",
                 stringsAsFactors = FALSE)

# Форматирование данных - удаление пробелов и замена запятых на точки
data$рождаем <- as.numeric(gsub(",", ".", data$рождаем))
data$смертн <- as.numeric(gsub(",", ".", data$смертн))
data$деск_см <- as.numeric(gsub(",", ".", data$деск_см))
data$длит_муж <- as.numeric(gsub(",", ".", data$длит_муж))
data$длит_жен <- as.numeric(gsub(",", ".", data$длит_жен))
data$доход <- as.numeric(gsub(" ", "", data$доход))

numeric_cols <- c("рождаем", "смертн", "деск_см", "длит_муж", "длит_жен", "доход")

# 2. Дескриптивный анализ
# Расчёт моды
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

desc_stats <- list()

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for (col in numeric_cols) {
  idx_min <- which.min(data[[col]])
  idx_max <- which.max(data[[col]])

  desc_stats[[col]] <- data.frame(
    Минимум = min(data[[col]], na.rm = TRUE),
    Страна_мин = data$страна[idx_min],
    Максимум = max(data[[col]], na.rm = TRUE),
    Страна_макс = data$страна[idx_max],
    Среднее = mean(data[[col]], na.rm = TRUE),
    Медиана = median(data[[col]], na.rm = TRUE),
    Мода = get_mode(na.omit(data[[col]]))
  )
  
  boxplot(data[[col]], main = paste("Боксплот для:", col), ylab = col)
}
par(mfrow = c(1, 1))

# Вывод результатов дескриптивного анализа
for (col in names(desc_stats)) {
  cat("\n=== Анализ для переменной:", col, "===\n")
  print(desc_stats[[col]])
}

# 3. Оценка оптимального числа кластеров по различным методам
# Метод локтя
data_numeric <- na.omit(data[, numeric_cols, drop = FALSE])
data_numeric_scaled <- scale(data_numeric)

fviz_nbclust(data_numeric_scaled, kmeans, method = "wss")

# Метод среднего силуэта
fviz_nbclust(data_numeric_scaled, kmeans, method = "silhouette")

# Статистика разрыва
gap_stat <- clusGap(data_numeric_scaled, FUN=kmeans, nstart = 1, K.max=10, B = 10)
fviz_gap_stat(gap_stat)

# Алгоритм на основе консенсуса
n_clust <- n_clusters(data_numeric, package=c("easystats", "NbClust", "mclust"), standardize = TRUE)
plot(n_clust)

# 4. Кластеризация при помощи дендрограммы
rownames(data_numeric_scaled) <- paste0("[", data$регион[complete.cases(data[numeric_cols])], "] ", 
                                        data$страна[complete.cases(data[numeric_cols])])
hc <- hclust(dist(data_numeric_scaled), method="ward.D2")
plot(hc, hang = -1)
rect.hclust(hc, k = 8, border = 2:9)

# 5. Столбчатые диаграммы и боксплоты
clusters <- cutree(hc, k = 8)

# Вывод участников каждого кластера
cluster_membership <- data.frame(
  Страна = data$страна[complete.cases(data[numeric_cols])],
  Регион = data$регион[complete.cases(data[numeric_cols])],
  Кластер = clusters
)

cluster_membership <- cluster_membership[order(cluster_membership$Кластер), ]

split(cluster_membership[, c("Страна", "Регион")], cluster_membership$Кластер)

# Столбчатая диаграмма
cluster_means <- aggregate(data_numeric_scaled, 
                           by = list(cluster = clusters), 
                           FUN = mean)

means_transposed <- t(cluster_means[, -1])
colnames(means_transposed) <- paste("Кластер", 1:8)

colors <- rainbow(nrow(means_transposed))

barplot(means_transposed,
        beside = TRUE,
        col = colors,
        legend.text = rownames(means_transposed),
        args.legend = list(x = "topright", bty = "n", cex = 0.8),
        main = "Средние значения параметров по кластерам",
        ylab = "Стандартизированные значения",
        xlab = "Кластеры")

# Боксплоты
cluster_data <- split(data.frame(data_numeric_scaled), clusters)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (i in 1:8) {
  boxplot(cluster_data[[i]], 
          main = paste("Кластер", i),
          ylab = "Значения",
          col = i + 1,
          las = 2,
          cex.axis = 0.8)
}

par(mfrow = c(1, 1))

# 6. Кластеризация k-means
km <- kmeans(data_numeric_scaled, centers = 8) 

fviz_cluster(km, data = data_numeric_scaled, 
             geom = "text",
             labelsize=8)

cluster_countries <- data.frame(
  Страна = rownames(data_numeric_scaled),
  Кластер = km$cluster
)

cluster_countries <- cluster_countries[order(cluster_countries$Кластер), ]

# Вывод стран в кластерах
for (i in 1:8) {
  cat("\n=== Кластер", i, "===\n")
  print(cluster_countries[cluster_countries$Кластер == i, "Страна"], quote = FALSE)
}

# 7. Скаттерплот для k-means
cluster_cols <- rainbow(8)[km$cluster]
pairs(data_numeric_scaled, 
      main = "Матрица scatterplot по кластерам",
      col = cluster_cols,
      oma = c(4, 4, 6, 25))

par(xpd = TRUE)
legend("right",
       legend = paste("Кластер", 1:8),
       col = rainbow(8),
       pch = 19,
       title = "Кластеры",
       cex = 1)  

# ЧАСТЬ 2. Классификация
# 1. Подготовка датасета
complete_rows <- complete.cases(data[, numeric_cols])

data$кластер <- NA
data$кластер[complete_rows] <- clusters
data$кластер <- as.factor(data$кластер)

data_clean <- data[!is.na(data$кластер), ]

set.seed(123)

train_index <- sample(1:nrow(data_clean), 
                      size = round(0.7 * nrow(data_clean)), 
                      replace = FALSE)

train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

train_data
test_data


# 2. Классификация наивным методом Байеса
# Обучение классификатора
train_filtered <- train_data[train_data$кластер != 5, ]
train_filtered

naive_data <- e1071::naiveBayes(кластер ~ ., data = train_filtered[, c(numeric_cols, "кластер")])
naive_data$tables
naive_data$tables$длит_муж

# Прогноз классификатора
pred <- predict(naive_data, newdata = test_data[, numeric_cols])

# Создание таблицы сопряженности
conf_matrix <- table(
  "Факт" = test_data$кластер,
  "Прогноз" = pred
)

print(conf_matrix)
acc <- mean(pred == test_data$кластер)
paste("Точность: ", round(100*acc, 2), "%", sep="")

# 3. Классификация деревьями решений
myFormula <- кластер ~ рождаем + смертн + деск_см + длит_муж + длит_жен + доход

countries_ctree <- party::ctree(myFormula, data=train_data)
plot(countries_ctree)

pred <- predict(countries_ctree, newdata=test_data)
table(Факт = test_data$кластер, Прогноз = pred)

acc <- mean(pred == test_data$кластер)
paste("Точность: ", round(100*acc, 2), "%", sep="")

# 4. Классификация случайным лесом
rf <- randomForest::randomForest(кластер ~ ., data=train_data, ntree=100, proximity=TRUE)

pred <- predict(rf, newdata = test_data)
conf_matrix <- table(
  "Факт" = test_data$кластер,
  "Прогноз" = pred
)
print(conf_matrix)
cat("Точность:", mean(pred == test_data$кластер) * 100, "%\n")
