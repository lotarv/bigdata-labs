x1 <- as.numeric(readline("Введите x1: "))
y1 <- as.numeric(readline("Введите y1: "))
x2 <- as.numeric(readline("Введите x2: "))
y2 <- as.numeric(readline("Введите y2: "))
N <- as.numeric(readline("Введите количество точек: "))

X <- runif(N, min=x1, max=x2)
Y <- runif(N, min=y1, max=y2)

#Создание матрицы координат
coords = cbind(X,Y)


plot(coords, pch = 16,col='orange',main="Случайное размещение точек в прямоугольнике")