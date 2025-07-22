#Install or load required packages
if (!require('MASS')) install.packages('MASS'); library(MASS)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('ellipse')) install.packages('ellipse'); library(ellipse)

#Creating Figure 2

#Parameters for the ellipse
rho <- 0.7
mean <- c(0, 0)
cov_matrix <- matrix(c(1, rho, rho, 1), 2, 2)

#Generate normally distributed bivariate data
set.seed(1401)
data <- mvrnorm(n = 1000, mu = mean, Sigma = cov_matrix)

#Create the ellipsoid object
ell<-ellipse(data, center = mean, scale=c(1,1), shape = cov_matrix, level = 0.90)

#Create a data frame from the generated data
data_frame <- data.frame(x = data[, 1], y = data[, 2])

#Draw the ellipsoid along the segmented plane
p <- ggplot(data_frame, aes(x, y)) +
  stat_ellipse(type = "norm", level = 0.95, color = "black", size=1, fill="gray") + # Ellipse
  geom_vline(xintercept = c(-1, 0, 1), color = "black", linetype="dashed") +
  geom_hline(yintercept = c(-1, 0, 1), color = "black", linetype="dashed") +
  annotate("rect", xmin = -Inf, xmax = -1, ymin = 1, ymax = Inf, alpha = 0.3, fill = "antiquewhite4", label = "a") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 1, ymax = Inf, alpha = 0.2, fill = "darkorchid4", label = "b") +
  annotate("rect", xmin = -Inf, xmax = -1, ymin = 0, ymax = 1, alpha = 0.2, fill = "darkorchid4", label = "c") +
  annotate("rect", xmin = 1, xmax = Inf, ymin = 0, ymax = -1, alpha = 0.2, fill = "darkorchid4", label = "d") +
  annotate("rect", xmin = 0, xmax = 1, ymin = -1, ymax = -Inf, alpha = 0.2, fill = "darkorchid4", label = "e") +
  annotate("rect", xmin = 1, xmax = Inf, ymin = -1, ymax = -Inf, alpha = 0.3, fill = "antiquewhite4", label = "f") +
  geom_text(aes(x = -2, y = 2, label = 'a'), color = "black") +
  geom_text(aes(x = -0.5, y = 2, label = 'b'), color = "black") +
  geom_text(aes(x = -2, y = 0.5, label = 'c'), color = "black") +
  geom_text(aes(x = 2, y = -0.5, label = 'd'), color = "black") +
  geom_text(aes(x = 0.5, y = -2, label = 'e'), color = "black") +
  geom_text(aes(x = 2, y = -2, label = 'f'), color = "black") +
  labs(x = "X Index z-scores",
       y = "Y Index z-scores")

#Print the plot
print(p)
#Recomended dimensions: 700x700

#Testing the odds of finding data points outside the ellipsoid and within the "f" quadrant

#Generate a dense cloud of bivariately distributed data
set.seed(1401)
n <- 100000
correlation <- 0.35
mean_vec <- c(0, 0)
cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)
data <- MASS::mvrnorm(n, mu = mean_vec, Sigma = cov_matrix)
df <- data.frame(x = data[, 1], y = data[, 2])
#Corroborate the data cloud approximates the desired proportion
cor.test(df$x, df$y)

#Create an ellipsoid to cover a certain proportion of the data
confidence_level <- 0.90
#Determine the proportions of the ellipsoid
eigen_decomp <- eigen(cov_matrix)
#Align the ellipsoid with the data cloud
rotation_matrix <- eigen_decomp$vectors
#Shrink the ellipsoid so that it covers 90% of the points
lengths <- sqrt(eigen_decomp$values * qchisq(confidence_level, 2))

#A function to check which points are inside the ellipse
check_inside <- function(x, y, center, lengths, rotation_matrix) {
  point <- matrix(c(x, y), nrow = 2)
  translated <- point - matrix(center, nrow = 2)
  rotated <- solve(rotation_matrix) %*% translated
  value <- sum((rotated / lengths)^2)
  return(value <= 1)
}

#Implement the function with the data frame containing the bivariate distribution
df$inside <- apply(df, 1, function(row) {
  check_inside(row['x'], row['y'], mean_vec, lengths, rotation_matrix)
})

#Count the number of points inside the ellipsoid
num_inside <- sum(df$inside)
print(paste("Number of points inside the ellipse:", num_inside))
#Note that the result approximates the 90% of the data points

#Visualization
ellipse_points <- data.frame(x = numeric(), y = numeric())
angles <- seq(0, 2 * pi, length.out = 100)

for(angle in angles) {
  point <- c(lengths[1] * cos(angle), lengths[2] * sin(angle))
  rotated_point <- rotation_matrix %*% point + mean_vec
  ellipse_points <- rbind(ellipse_points, data.frame(x = rotated_point[1], y = rotated_point[2]))
}

#Wait ofr it; the display is somewhat delayed due to the amount of data points
ggplot(df, aes(x = x, y = y, color = inside)) +
  geom_point(alpha = 0.1) +
  geom_path(data = ellipse_points, aes(x = x, y = y), color = "red") +
  ggtitle("Points Inside Ellipse") +
  theme_minimal()

#This is an alternative visualization (it also takes some time to load) 
plot(df$x[which(df$inside==FALSE)], df$y[which(df$inside==FALSE)])

length(df$inside[which(df$inside==FALSE)])
#Count the data points that fall both outside the ellipsoid and within the "f" quadrant
cases <- length(df$inside[which(df$inside==FALSE & df$x > 1 & df$y > 1)]) +
length(df$inside[which(df$inside==FALSE & df$x > 1 & df$y < -1)]) +
length(df$inside[which(df$inside==FALSE & df$x < -1 & df$y > 1)]) +
length(df$inside[which(df$inside==FALSE & df$x < -1 & df$y < -1)])
#This is the proportion of theoretical cases that met that criteria
cases/100000
#And this is the proportion of observed data meeting the criteria
1/16

#Data analysis for the original study es finished
