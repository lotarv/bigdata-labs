# Загрузка нужных библиотек
library(ggplot2)

# Функция для расчета моды
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Чтение CSV-файла с учетом формата
data <- read.csv2("trips.csv", 
                  fileEncoding = "UTF-8", 
                  na.strings = c("", "NA"))

# Приведение колонок к нужным типам
numeric_cols <- c("Days", "Visits", "ExtrCharges", "Tips", "USD")
for (col in numeric_cols) {
  data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
}

# Переименование колонок (опционально, для удобства)
colnames(data) <- c("Регион", "Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")

# Дескриптивный анализ + боксплоты
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))  # сетка
desc_stats <- list()

for (col in c("Длительность", "Визиты", "ДопРасходы", "Чаевые", "Доход")) {
  x <- data[[col]]
  idx_min <- which.min(x)
  idx_max <- which.max(x)
  
  desc_stats[[col]] <- data.frame(
    Минимум = min(x, na.rm = TRUE),
    Максимум = max(x, na.rm = TRUE),
    Среднее = mean(x, na.rm = TRUE),
    Медиана = median(x, na.rm = TRUE),
    Мода = get_mode(na.omit(x))
  )
  
  
  boxplot(x, main = paste("Боксплот для:", col), ylab = col)
}
par(mfrow = c(1, 1))  # сброс макета

# Печать статистик
for (col in names(desc_stats)) {
  cat("\n===", col, "===\n")
  print(desc_stats[[col]])
}



