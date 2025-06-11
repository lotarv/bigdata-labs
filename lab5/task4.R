#Задание 4


numeric_data <- na.omit(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")])
scaled_data <- scale(numeric_data)


labels <- paste0("[", data$Регион[complete.cases(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")])], "] ",
                 seq_len(nrow(numeric_data)))


hc <- hclust(dist(scaled_data), method = "ward.D2")


plot(hc, labels = labels, hang = -1, main = "Дендрограмма иерархической кластеризации")


rect.hclust(hc, k = 3, border = 2:4)
