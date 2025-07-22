

set.seed(1401)
n <- 100000
correlation <- 0.35
mean_vec <- c(0, 0)
cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)
data <- MASS::mvrnorm(n, mu = mean_vec, Sigma = cov_matrix)
df <- data.frame(x = data[, 1], y = data[, 2])

cases <- length(which(df$x < -1 & df$y > 1)) + length(which(df$x > 1 & df$y < -1))
cases/100000
1/16
