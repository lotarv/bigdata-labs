labels <- paste0("[", data$Регион[complete.cases(data[, c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")])], "] #", seq_len(nrow(scaled_data)))

set.seed(42)
km_result <- kmeans(scaled_data, centers = 3, nstart = 25)

library(factoextra)

p_labeled <- fviz_cluster(km_result, data = scaled_data,
                          geom = "text",
                          labelsize = 4,
                          ellipse.type = "convex",
                          palette = "jco",
                          ggtheme = theme_minimal(),
                          repel = TRUE) +
  geom_text(aes(label = labels), check_overlap = TRUE)

print(p_labeled)
