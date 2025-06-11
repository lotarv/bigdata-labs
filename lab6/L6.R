library(car)

# Загрузка данных
data <- read.csv("athlete_events.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE,
                 na.strings = c("NA", ""),
                 quote = "\"")

# Удаляем дубликаты по ID (берём одного спортсмена)
data <- data[!duplicated(data$ID), ]

# Выборка по видам спорта
speed_skating <- data[data$Sport == "Speed Skating", ]
figure_skating <- data[data$Sport == "Figure Skating" & data$Sex == "M" & !is.na(data$Weight), ]
speed_men <- data[data$Sport == "Speed Skating" & data$Sex == "M" & !is.na(data$Weight), ]

# Общая функция для построения гистограмм и boxplot'ов
get_limits <- function(var) {
  range(na.omit(c(data[[var]], speed_skating[[var]])))
}

age_lim <- get_limits("Age")
height_lim <- get_limits("Height")
weight_lim <- get_limits("Weight")

plot_pair <- function(var, title, unit, x_lim) {
  par(mfrow = c(2, 2))
  hist(speed_skating[[var]], main = paste(title, "(конькобежцы)"), 
       xlab = unit, col = "lightblue", xlim = x_lim)
  hist(data[[var]], main = paste(title, "(все атлеты)"), 
       xlab = unit, col = "lightgreen", xlim = x_lim)
  
  boxplot(speed_skating[[var]], main = paste(title, "(конькобежцы)"), 
          ylab = unit, ylim = x_lim)
  boxplot(data[[var]], main = paste(title, "(все атлеты)"), 
          ylab = unit, ylim = x_lim)
  par(mfrow = c(1, 1))
}

# Построение графиков
plot_pair("Age", "Возраст", "лет", age_lim)
plot_pair("Height", "Рост", "см", height_lim)
plot_pair("Weight", "Вес", "кг", weight_lim)

# Сводка по описательной статистике
cat("Конькобежцы:\n")
print(summary(speed_skating[, c("Age", "Height", "Weight")]))
cat("\nВсе атлеты:\n")
print(summary(data[, c("Age", "Height", "Weight")]))

# Проверка нормальности веса у конькобежцев
speed_weights <- na.omit(speed_men$Weight)

cat("\nТест Шапиро–Уилка:\n")
shapiro_result <- shapiro.test(speed_weights)
print(shapiro_result)

# QQ-график веса конькобежцев
qqPlot(speed_weights, main = "QQ-график: Вес конькобежцев")

# Сравнение с нормальным распределением
v <- rnorm(length(speed_weights), mean = mean(speed_weights), sd = sd(speed_weights))
qqPlot(v, main = "QQ-график: Нормальное распределение")

# Проверка гипотезы о медиане (Уилкоксон)
cat("\nТест Уилкоксона (гипотеза о медиане):\n")
print(wilcox.test(speed_weights, mu = median(speed_weights), conf.int = TRUE))

# Проверка гипотезы о среднем значении (t-тест)
cat("\nТест Стьюдента (гипотеза о среднем):\n")
print(t.test(speed_weights, mu = mean(speed_weights), conf.int = TRUE))


# Подготовка данных для сравнения
weights_df <- data.frame(
  Weight = c(speed_men$Weight, figure_skating$Weight),
  Sport = c(rep("Speed Skating", length(speed_men$Weight)), 
            rep("Figure Skating", length(figure_skating$Weight)))
)

# Тест Бартлетта
print(bartlett.test(Weight ~ Sport, data = weights_df))

