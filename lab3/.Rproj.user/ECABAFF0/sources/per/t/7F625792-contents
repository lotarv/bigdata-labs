men_data <- readxl::read_excel("men_data.xlsx")
women_data <- readxl::read_excel("women_data.xlsx")

# Дескриптивный анализ
convert_to_places <- function(df) {
  places <- c()
  for(i in 1:nrow(df)) {
    for(place in 1:8) {
      col_name <- switch(place,
                         "1" = "Золото",
                         "2" = "Серебро",
                         "3" = "Бронза",
                         "4" = "Четвертый",
                         "5" = "Пятый",
                         "6" = "Шестой",
                         "7" = "Седьмой",
                         "8" = "Восьмой")
      count <- df[i, col_name]
      places <- c(places, rep(place, count))
    }
  }
  return(places)
}

men_places <- convert_to_places(men_data)
women_places <- convert_to_places(women_data)

cat("Мужчины - лучшее место:", min(men_places), "\n")
cat("Женщины - лучшее место:", min(women_places), "\n\n")

cat("Мужчины - худшее место:", max(men_places), "\n")
cat("Женщины - худшее место:", max(women_places), "\n\n")

cat("Мужчины - среднее место:", mean(men_places), "\n")
cat("Женщины - среднее место:", mean(women_places), "\n\n")

par(mfrow = c(1, 2))
hist(men_places, breaks = 0:8 + 0.5, col = "blue",
     main = "Мужчины: распределение мест",
     xlab = "Место", ylab = "Частота", xlim = c(1, 8))
hist(women_places, breaks = 0:8 + 0.5, col = "red",
     main = "Женщины: распределение мест",
     xlab = "Место", ylab = "Частота", xlim = c(1, 8))
par(mfrow = c(1, 1))

boxplot(list(Мужчины = men_places, Женщины = women_places),
        col = c("blue", "red"),
        main = "Распределение занятых мест",
        ylab = "Место", ylim = c(1, 8))
# Конец дескриптивного анализа

# ЗАДАНИЕ 2

# Столбчатая диаграмма
combined_counts <- men_data[, c("Золото", "Серебро", "Бронза", "Четвертый", "Пятый", "Шестой", "Седьмой", "Восьмой")] +
  women_data[, c("Золото", "Серебро", "Бронза", "Четвертый", "Пятый", "Шестой", "Седьмой", "Восьмой")]
rownames(combined_counts) <- men_data$Год

barplot(
  t(combined_counts),
  beside = TRUE,
  col = rainbow(ncol(combined_counts)),
  main = "Суммарное количество призовых мест по Олимпиадам (Мужчины + Женщины)",
  xlab = "Год Олимпиады",
  ylab = "Количество",
  legend.text = colnames(combined_counts),
  args.legend = list(x = "topright", bty = "n")
)

# Круговая диаграмма
combined_gold <- men_data$Золото + women_data$Золото

names(combined_gold) <- men_data$Год
combined_gold <- combined_gold[combined_gold > 0]
pie(
  combined_gold,
  main = "Суммарное количество золотых медалей по Олимпиадам (мужчины и женщины)",
  col = rainbow(length(combined_gold))
)

# Функциональный график
years <- men_data$Год
men_total <- men_data$Всего
women_total <- women_data$Всего

plot(
  years, men_total,
  type = "o",
  col = "blue",
  ylim = range(c(men_total, women_total)),
  main = "Динамика призовых мест (мужчины и женщины)",
  xlab = "Год",
  ylab = "Количество мест"
)
lines(years, women_total, type = "o", col = "red")
legend(
  "topright",
  legend = c("Мужчины", "Женщины"),
  col = c("blue", "red"),
  lty = 1,
  pch = 1
)

# ЗАДАНИЕ 3

countries <- c("Канада", "Финляндия", "Германия", "Нидерланды", "Норвегия", "Швеция", "США")

gold_data <- list()
prize_data <- list()

for (country in countries) {
  file_name <- paste0(country, "_data.xlsx")
  data <- readxl::read_excel(file_name)
  
  gold_data[[country]] <- data$Золото
  
  prize_data[[country]] <- data$Золото + data$Серебро + data$Бронза
}

years <- data$Год
gold_matrix <- do.call(cbind, gold_data)
prize_matrix <- do.call(cbind, prize_data)

# Золотые медали
matplot(
  years, gold_matrix,
  type = "o",
  pch = 16,
  col = 1:length(countries),
  lty = 1,
  xlab = "Год Олимпиады",
  ylab = "Количество золотых медалей",
  main = "Количество золотых медалей по странам (2004-2024)"
)
legend(
  "topright",
  legend = countries,
  col = 1:length(countries),
  lty = 1,
  pch = 16,
  bty = "n"
)

# Призовые места
matplot(
  years, prize_matrix,
  type = "o",
  pch = 16,
  col = 1:length(countries),
  lty = 1,
  xlab = "Год Олимпиады",
  ylab = "Количество призовых мест (1-3)",
  main = "Количество призовых мест по странам (2004-2024)"
)
legend(
  "topright",
  legend = countries,
  col = 1:length(countries),
  lty = 1,
  pch = 16,
  bty = "n"
)

# ЗАДАНИЕ 4

# Столбчатая диаграмма
prize_categories <- c("Золото", "Серебро", "Бронза", "Четвертый", "Пятый", "Шестой", "Седьмой", "Восьмой")
men_counts <- colSums(men_data[, prize_categories])
women_counts <- colSums(women_data[, prize_categories])

barplot(
  rbind(men_counts, women_counts),
  beside = TRUE,
  col = c("blue", "red"),
  names.arg = prize_categories,
  xlab = "Призовые места",
  ylab = "Количество",
  main = "Сравнение призовых мест по конькобежному спорту (США)",
  legend.text = c("Мужчины", "Женщины"),
  args.legend = list(x = "topright", bty = "n")
)

# Круговая диаграмма
total_men_prizes <- sum(men_counts)
total_women_prizes <- sum(women_counts)
prize_shares <- c(total_men_prizes, total_women_prizes)
names(prize_shares) <- c("Мужчины", "Женщины")

pie(
  prize_shares,
  col = c("blue", "red"),
  main = "Доля призовых мест по конькобежному спорту (США)"
)

# Устанавливаем сетку 2 на 3
par(mfrow = c(2, 3))

# Предполагаем, что у тебя есть переменные:
# men_data и women_data с колонками: Год, Золото, Серебро, ..., Восьмой

years <- men_data$Год
places <- c("Золото", "Серебро", "Бронза", "Четвертый", "Пятый", "Шестой", "Седьмой", "Восьмой")

for (i in 2:length(years)) {
  men_row <- as.numeric(men_data[i, places])
  women_row <- as.numeric(women_data[i, places])
  
  counts <- rbind(men_row, women_row)
  
  barplot(
    counts,
    beside = TRUE,
    col = c("blue", "red"),
    ylim = c(0, 3),
    names.arg = 1:8,
    main = paste("Олимпиада", years[i]),
    xlab = "Место",
    ylab = "Количество"
  )
  
  legend("topright", legend = c("Мужчины", "Женщины"), fill = c("blue", "red"), bty = "n")
    
}

# Возврат к одному графику
par(mfrow = c(1, 1))

