# Задание 1


numeric_cols <- c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")


complete_rows <- complete.cases(data[, numeric_cols])
data_mod <- data[complete_rows, ]

data_scaled <- scale(data_mod[, numeric_cols])

hc <- hclust(dist(data_scaled), method = "ward.D2")

clusters <- cutree(hc, k = 3)

data_mod$cluster <- factor(clusters)

set.seed(123)
train_idx  <- sample(seq_len(nrow(data_mod)), size = round(0.7 * nrow(data_mod)))
train_data <- data_mod[train_idx, ]
test_data  <- data_mod[-train_idx, ]

#ЗАДАНИЕ 2 наивный байес

library(e1071)

nb_model <- naiveBayes(cluster ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход,
                       data = train_data)

duration_stats <- nb_model$tables$Доход
print(duration_stats)

# Задание 3 Наивный байес

pred_nb <- predict(nb_model, newdata = test_data[, numeric_cols])

conf_matrix_nb <- table(Факт = test_data$cluster, Прогноз = pred_nb)
print(conf_matrix_nb)

acc_nb <- mean(pred_nb == test_data$cluster)
cat("Точность (наивный байес): ", round(acc_nb * 100, 2), "%\n", sep = "")


# Задание 4 Граф дерева

library(party)

formula_tree <- cluster ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход


tree_model <- ctree(formula_tree, data = train_data)

plot(tree_model,
     main = "Дерево решений для кластеров (k = 3)",
     type = "simple")



# Задание 5 дерево решений

tree_pred <- predict(tree_model, newdata = test_data)

conf_matrix_tree <- table(Факт = test_data$cluster, Прогноз = tree_pred)
print(conf_matrix_tree)

acc_tree <- mean(tree_pred == test_data$cluster)
cat("Точность (дерево решений): ", round(acc_tree * 100, 2), "%\n", sep = "")

# Задание 6 случайный лес

library(randomForest)

# Обучаем случайный лес на train_data (100 деревьев)
rf_model <- randomForest(cluster ~ Длительность + Визиты + ДопРасходы + Чаевые + Доход,
                         data = train_data, ntree = 100)

# Делаем прогноз на test_data
rf_pred <- predict(rf_model, newdata = test_data)

# Составляем матрицу ошибок и выводим её
conf_matrix_rf <- table(Факт = test_data$cluster, Прогноз = rf_pred)
print(conf_matrix_rf)

# Вычисляем и выводим точность
acc_rf <- mean(rf_pred == test_data$cluster)
cat("Точность (случайный лес):", round(acc_rf * 100, 2), "%\n")



