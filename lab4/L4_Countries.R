library(rvest)

countries_ru <- c("Корея", "Румыния", "Чехия", "Китай", "Япония")
countries_en <- c("South Korea", "Romania", "Czech Republic", "China", "Japan")
names(countries_en) <- countries_ru 


yearly_data <- list()

for (year in 2014:2021) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  page <- read_html(url)
  
  table <- page %>% html_nodes("table#t2") %>% html_table() %>% .[[1]]
  table <- table[-1, -1]
  
  colnames(table) <- c("Страна", "Индекс качества жизни", "Индекс покупательной способности", 
                       "Индекс безопасности", "Индекс здравоохранения", "Индекс стоимости жизни", 
                       "Соотношение цены недвижимости к доходу", "Индекс времени в пути", 
                       "Индекс загрязнения", "Индекс климата")
  
  filtered <- table[table$Страна %in% countries_en, ]
  filtered$Страна <- names(countries_en)[match(filtered$Страна, countries_en)]
  
  numeric_cols <- setdiff(names(filtered), "Страна")
  filtered[numeric_cols] <- lapply(filtered[numeric_cols], function(x) {
    x <- gsub(",", "", x)
    x <- gsub("[^0-9\\.]", "", x)
    as.numeric(x)
  })
  
  yearly_data[[as.character(year)]] <- filtered
  Sys.sleep(1)
}

# Объединение всех лет в таблицу
all_data <- do.call(rbind, lapply(names(yearly_data), function(year) {
  df <- yearly_data[[year]]
  df$Год <- as.numeric(year)
  df
}))


all_data <- all_data[!(all_data$Год %in% 2014:2015 & is.na(all_data$`Индекс климата`)), ]

индексы <- setdiff(names(all_data), c("Страна", "Год"))

# Мода
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Дескриптивная статистика
desc_stats <- lapply(индексы, function(index) {
  values <- all_data[[index]]
  data.frame(
    Индекс = index,
    Среднее = mean(values, na.rm = TRUE),
    Медиана = median(values, na.rm = TRUE),
    Мода = get_mode(round(values, 1)),
    Мин = min(values, na.rm = TRUE),
    Макс = max(values, na.rm = TRUE),
    СКО = sd(values, na.rm = TRUE)
  )
})

stats_table <- do.call(rbind, desc_stats)
print(stats_table)

# Графики
index_names <- names(yearly_data[["2014"]])[-1]

par(family = "sans", mfrow = c(3, 3), mar = c(4, 4, 2, 1))

colors <- c("Корея" = "red", 
            "Румыния" = "blue", 
            "Чехия" = "green", 
            "Китай" = "purple", 
            "Япония" = "orange")

for (index in setdiff(index_names, "Индекс климата")) {
  y_range <- range(sapply(yearly_data, function(df) range(df[[index]], na.rm = TRUE)))
  
  plot(NA, 
       xlim = c(2014, 2021), 
       ylim = c(0, y_range[2]),
       xlab = "Год", 
       ylab = "Значение индекса",
       main = index,
       las = 1)
  
  for (country in countries_ru) {
    values <- sapply(yearly_data, function(df) {
      df[df$Страна == country, index]
    })
    
    lines(2014:2021, values, 
          type = "l", 
          col = colors[country], 
          pch = 19, 
          lwd = 2)
  }
  
  legend("bottomright", 
         legend = countries_ru, 
         col = colors, 
         lty = 1, 
         pch = 19,
         cex = 1.2,
         bty = "n")
}

# Индекс климата (2016–2021)
climate_y_range <- range(sapply(yearly_data[as.character(2016:2021)], 
                                function(df) range(df[["Индекс климата"]], na.rm = TRUE)))

plot(NA, 
     xlim = c(2016, 2021), 
     ylim = c(0, climate_y_range[2]),
     xlab = "Год", 
     ylab = "Индекс климата",
     main = "Динамика индекса климата (2016–2021)",
     las = 1)

for (country in countries_ru) {
  values <- sapply(yearly_data[as.character(2016:2021)], function(df) {
    df[df$Страна == country, "Индекс климата"]
  })
  
  lines(2016:2021, values, 
        type = "l", 
        col = colors[country], 
        pch = 19, 
        lwd = 2)
}

par(mfrow = c(1, 1))
