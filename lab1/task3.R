#1
n1 <- 1:50
sequence1 <- 1 / (n1 * (n1 + 1))
cat("Sum of 1st sequence:", sum(sequence1), "\n")
#2
n2 <- 2^(0:20)
sequence2 <- 1 / n2
cat("Sum of 2nd sequence:", sum(sequence2), "\n")
#3
n3_1 <- seq(1,28,by=3)
n3_2 <- 3^(0:9)
sequence3 <- n3_1 / n3_2
cat("Sum of 3rd sequence:", sum(sequence3), "\n")

cat("Amount of numbers more than 0.5 in third seuqnce: ", sum(sequence3 > 0.5), "\n")


