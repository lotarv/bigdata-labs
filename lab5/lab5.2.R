library(factoextra)
library(cluster)
library(parameters)
library(NbClust)
library(mclust)
library(easystats)
library(e1071)
library(party)
library(randomForest)
library(caret)

# Предполагается, что у тебя есть таблица data с нужными столбцами:
# Регион, Длительность, Визиты, ДопРасходы, Чаевые, Доход

numeric_cols <- c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")

# Очищаем данные по числовым колонкам
complete_rows <- complete.cases(data[, numeric_cols])

# Используем иерархическую кластеризацию, clusters из неё
clusters <- cutree(hc, k = 8)

# Добавляем кластер в таблицу
data$кластер <- NA
data$кластер[complete_rows] <- clusters
data$кластер <- as.factor(data$кластер)

# Оставляем только наблюдения с заполненным кластером
data_clean <- data[!is.na(data$кластер), ]

# Удаляем кластер 5 (одиночка)
data_filtered <- subset(data_clean, кластер != 5)

# Разделяем на train/test с стратификацией, чтобы в выборках были все кластеры
set.seed(123)
trainIndex <- createDataPartition(data_filtered$кластер, p = 0.7, list = FALSE)
train_data <- data_filtered[trainIndex, ]
test_data <- data_filtered[-trainIndex, ]

# Наивный Байес
naive_data <- naiveBayes(кластер ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход, data = train_data)
print(naive_data$tables) # таблицы вероятностей

pred_nb <- predict(naive_data, newdata = test_data[, numeric_cols])
conf_matrix_nb <- table(Факт = test_data$кластер, Прогноз = pred_nb)
print(conf_matrix_nb)
cat("Точность (наивный байес):", round(mean(pred_nb == test_data$кластер) * 100, 2), "%\n")

# Дерево решений
formula_ctree <- кластер ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход
ctree_model <- ctree(formula_ctree, data = train_data)
plot(ctree_model)

pred_ctree <- predict(ctree_model, newdata = test_data)
conf_matrix_ctree <- table(Факт = test_data$кластер, Прогноз = pred_ctree)
print(conf_matrix_ctree)
cat("Точность (дерево решений):", round(mean(pred_ctree == test_data$кластер) * 100, 2), "%\n")

# Фильтрация train_data для случайного леса — оставляем только непустые классы
table_train <- table(train_data$кластер)
valid_clusters <- names(table_train[table_train > 0])
train_data_filtered <- subset(train_data, кластер %in% valid_clusters)

# Подсчёт количества объектов в кластерах
table_clusters <- table(data_filtered$кластер)

# Исключаем кластеры с менее чем 5 объектами (например)
valid_clusters <- names(table_clusters[table_clusters >= 5])

# Фильтрация исходных данных
data_final <- subset(data_filtered, кластер %in% valid_clusters)

# Разбиение train/test
set.seed(123)
trainIndex <- createDataPartition(data_final$кластер, p = 0.7, list = FALSE)
train_data <- data_final[trainIndex, ]
test_data <- data_final[-trainIndex, ]

# Дальше обучаем модели на train_data и тестируем на test_data

# Случайный лес
rf_model <- randomForest(кластер ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход,
                         data = train_data_filtered, ntree = 100, proximity = TRUE)

pred_rf <- predict(rf_model, newdata = test_data)
conf_matrix_rf <- table(Факт = test_data$кластер, Прогноз = pred_rf)
print(conf_matrix_rf)
cat("Точность (случайный лес):", round(mean(pred_rf == test_data$кластер) * 100, 2), "%\n")


