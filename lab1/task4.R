vec3 = seq(3,27,by=3)
selected_values = vec3[c(2,5,7)]
cat("2,6 and 6 values: ", selected_values, "\n")

penultimate= vec3[length(vec3) - 1]
cat("Penultimate value: ", penultimate, "\n")

all_except_penultimate = vec3[-(length(vec3) - 1)]
cat("All except penultimate: ", all_except_penultimate, "\n")

all_except_sixth = vec3[-6]
cat("All except 6th: ", all_except_sixth, "\n")

all_except_first_and_last = vec3[c(-1, -(length(vec3)))]
cat("All except first and last: ", all_except_first_and_last, "\n")

more_than_4_less_than_10 = vec3[vec3 > 4 & vec3 < 10]
cat("Values more than 4 but less than 10: ", more_than_4_less_than_10, "\n")

less_than_4_or_more_than_10 = vec3[vec3 < 4 | vec3 > 10]
cat("Values less than 4 or more than 10: ", less_than_4_or_more_than_10, "\n")

