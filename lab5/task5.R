#Задание 5


cluster_cut <- cutree(hc, k = 3)


clustered_data <- na.omit(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")])
clustered_data$Кластер <- as.factor(cluster_cut)


scaled_means <- clustered_data
scaled_means[, 1:5] <- scale(scaled_means[, 1:5])  


library(reshape2)
library(ggplot2)

means_scaled <- aggregate(. ~ Кластер, data = scaled_means, FUN = mean)
means_scaled_melted <- melt(means_scaled, id.vars = "Кластер")


p_scaled <- ggplot(means_scaled_melted, aes(x = variable, y = value, fill = Кластер)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Средние стандартизованные значения параметров по кластерам",
       x = "Параметры", y = "Z-оценка") +
  theme_minimal()

print(p_scaled)

melted_box <- melt(clustered_data, id.vars = "Кластер")

p_box <- ggplot(melted_box, aes(x = Кластер, y = value, fill = Кластер)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplot распределения признаков по кластерам",
       x = "Кластер", y = "Значение") +
  theme_minimal()

print(p_box)
