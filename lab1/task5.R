v = sample(1:100, 100,replace=TRUE)

average = sum(v) / length(v)

values_less_than_average = v[v < average]

matrix = matrix(v, 10,10)

even_rows = matrix[seq(2, nrow(matrix), by=2), ]

odd_rows = matrix[seq(1, nrow(matrix), by=2), ]


cat("Среднее арифметическое: ", average, "\n")
cat("Элементы, меньшие среднего арифметического: ", values_less_than_average)
print("")
print("Матрица 10х10:")
print(matrix)
print("Векторы четных строк:")
print(even_rows)
print("Векторы нечетных строк:")
print(odd_rows)
