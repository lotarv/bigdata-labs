# Задание 7

scaled_data <- scale(na.omit(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")]))


cluster_labels <- as.numeric(km_result$cluster)


colors <- ifelse(cluster_labels == 1, "red",
                 ifelse(cluster_labels == 2, "green", "blue"))


par(mar = c(4, 4, 4, 8))  # нижняя, левая, верхняя, правая

pairs(scaled_data,
      main = "Матрица scatterplot по кластерам",
      col = colors,
      pch = 19)

par(xpd = TRUE)
legend("topright",
       inset = c(0.95, -0.05),
       legend = c("Кластер 1", "Кластер 2", "Кластер 3"),
       col = c("red", "green", "blue"),
       pch = 19,
       title = "Кластеры",
       bty = "n")
par(xpd = FALSE)
