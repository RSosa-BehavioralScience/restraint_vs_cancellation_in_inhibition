#=============================================================================
#RUNS TEST FOR LINEARITY ASSESSMENT
#=============================================================================
#Purpose: Assess whether linear modeling assumptions are appropriate for 
#         RCI-RRI relationships at both within-subject and between-subject 
#         levels
#
#Rationale: The runs test detects systematic clustering of same-sign residuals
#           along the predictor variable. Under linearity, residuals should be
#           randomly distributed. Fewer runs than expected indicates clustering
#           and suggests non-linear relationships.
#
#Method: 1. Fit linear model to each data set
#        2. Order residuals by predictor values (RRI)
#        3. Count consecutive sequences of same-sign residuals ("runs")
#        4. Compare observed vs expected runs under random distribution
#=============================================================================

#Set the R console language to English to make troubleshooting and help seeking 
#more straightforward
Sys.setenv(LANG = "en")

#Load required packages
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('boot')) install.packages('boot'); library(boot)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)

#Prepare target data set
RvsC85_last$RRI <- as.numeric(RvsC85_last$RRI)
RvsC85_last$RCI <- as.numeric(RvsC85_last$RCI)
clean_data <- na.omit(RvsC85_last)

#=============================================================================
#RUNS TEST FOR INDIVIDUAL RATS
#=============================================================================
#Rationale: Tests whether residuals from linear fit show systematic clustering
#           when ordered by predictor values. Random residuals should alternate
#           frequently between positive and negative, producing many "runs".
#           Clustering produces fewer runs than expected by chance.

#Helper function runs test on residuals from linear RCI ~ RRI fit for single rat
#Arguments: data frame with RCI and RRI columns for one rat
#Returns: Vector: [observed_runs, expected_runs, p_value]
within_rat_runs <- function(rat_data) {
  #Fit linear model and extract residuals
  linear_fit <- lm(RCI ~ RRI, data = rat_data)
  resids <- residuals(linear_fit)
  
  #Order residuals by RRI values (critical for spatial runs test)
  order_idx <- order(rat_data$RRI)
  ordered_resids <- resids[order_idx]
  
  #Convert residuals to signs (+1 for positive, -1 for negative)
  residual_signs <- sign(ordered_resids)
  
  #Count runs using run-length encoding
  #A "run" is a sequence of consecutive same-sign residuals
  runs <- rle(residual_signs)
  observed_runs <- length(runs$lengths)
  
  #Calculate components for expected runs formula
  n_pos <- sum(residual_signs == 1)   #Number of positive residuals
  n_neg <- sum(residual_signs == -1)  #Number of negative residuals
  n_total <- length(residual_signs)
  
  #Expected runs under random distribution
  #Formula: E(R) = (2*n_pos*n_neg)/n_total + 1
  expected_runs <- ((2 * n_pos * n_neg) / n_total) + 1
  
  #Variance of runs under null hypothesis of randomness
  variance_runs <- (2 * n_pos * n_neg * (2 * n_pos * n_neg - n_total)) / 
    (n_total^2 * (n_total - 1))
  
  #Z-test for departure from expected runs
  z_score <- (observed_runs - expected_runs) / sqrt(variance_runs)
  
  #Two-tailed p-value (testing for any systematic pattern)
  p_value <- 2 * pnorm(abs(z_score), lower.tail = FALSE)
  
  return(c(observed_runs, expected_runs, p_value))
}

#Apply runs test to each rat separately
within_runs_results <- t(sapply(split(clean_data, clean_data$rat), 
                                within_rat_runs))
colnames(within_runs_results) <- c("observed_runs", "expected_runs", "p_value")

#Display individual rat results
print(within_runs_results)

#Extract complete cases for statistical analysis
valid_obs <- within_runs_results[,"observed_runs"]
valid_exp <- within_runs_results[,"expected_runs"]
complete_cases <- !is.na(valid_obs) & !is.na(valid_exp)

observed_clean <- valid_obs[complete_cases]
expected_clean <- valid_exp[complete_cases]

cat("\n=== WITHIN-SUBJECT STATISTICAL SUMMARY ===\n")
cat(sprintf("Mean observed runs: %.2f (SD = %.2f)\n", 
            mean(observed_clean), sd(observed_clean)))
cat(sprintf("Mean expected runs: %.2f (SD = %.2f)\n", 
            mean(expected_clean), sd(expected_clean)))
cat(sprintf("Mean difference (obs - exp): %.2f\n", 
            mean(observed_clean - expected_clean)))

#One-sided paired t-test: Are observed runs significantly FEWER than expected?
#Rationale: Clustering produces fewer runs, so we test H1: observed < expected
t_test_within <- t.test(observed_clean, expected_clean, 
                        alternative = "less", paired = TRUE)

cat(sprintf("One-sided paired t-test: t(%d) = %.3f, p = %.4f\n", 
            t_test_within$parameter, 
            t_test_within$statistic, 
            t_test_within$p.value))

#Count individual rats showing significant clustering
valid_p <- within_runs_results[,"p_value"]
significant_rats <- sum(valid_p < 0.05, na.rm = TRUE)
total_valid <- sum(!is.na(valid_p))
cat(sprintf("Individual rats with significant clustering: %d/%d\n", 
            significant_rats, total_valid))

#=============================================================================
#BETWEEN-SUBJECT ANALYSIS (RAT AVERAGES)
#=============================================================================
#Rationale: Tests whether the relationship between rat-average RCI and RRI
#           shows systematic clustering, addressing between-subject linearity

#Calculate rat averages for between-subject analysis
rat_means <- aggregate(cbind(RCI, RRI) ~ rat, data = clean_data, FUN = mean)

#Bootstrap function for runs test uncertainty estimation
#Returns: confidence intervals for difference between observed and expected runs
boot_runs_test <- function(data, indices) {
  
  boot_data <- data[indices, ]
  
  #Apply runs test to bootstrap sample
  linear_fit <- lm(RCI ~ RRI, data = boot_data)
  resids <- residuals(linear_fit)
  
  order_idx <- order(boot_data$RRI)
  ordered_resids <- resids[order_idx]
  residual_signs <- sign(ordered_resids)
  
  runs <- rle(residual_signs)
  observed_runs <- length(runs$lengths)
  
  n_pos <- sum(residual_signs == 1)
  n_neg <- sum(residual_signs == -1)
  n_total <- length(residual_signs)
  
  if(n_pos > 0 & n_neg > 0) {
    expected_runs <- ((2 * n_pos * n_neg) / n_total) + 1
    return(observed_runs - expected_runs)  #Return difference for CI estimation
  } else {
    return(NA)
  }
}

#Perform bootstrap analysis for rat averages
set.seed(1401)  # For reproducibility
boot_results <- boot(rat_means, boot_runs_test, R = 1000)

#Calculate bootstrap confidence interval
boot_ci <- boot.ci(boot_results, type = "perc", conf = 0.95)

#Report bootstrap results
cat("Bootstrap analysis results:\n")
cat(sprintf("Bootstrap mean difference (obs - exp): %.3f\n", 
            mean(boot_results$t, na.rm = TRUE)))
cat(sprintf("95%% Bootstrap CI: [%.3f, %.3f]\n", 
            boot_ci$percent[4], boot_ci$percent[5]))

#=============================================================================
#DATA VISUALIZATION
#=============================================================================

#Prepare data for plotting
rat_means$residuals <- residuals(lm(RCI ~ RRI, data = rat_means))
rat_means$fitted <- fitted(lm(RCI ~ RRI, data = rat_means))
rat_means$residual_sign <- factor(ifelse(rat_means$residuals > 0, 
                                         "Positive", 
                                         "Negative"))

#Order by RRI for spatial runs visualization
rat_means <- rat_means[order(rat_means$RRI), ]

#Runs Test Visualization for Rat-Average Data
runs_plot <- ggplot(rat_means, aes(x = RRI, y = RCI)) +
  #Linear fit line
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "black", 
              linetype = "solid", 
              size = 1) +
  
  #Residual lines (vertical dashed lines from fitted to observed)
  geom_segment(aes(x = RRI, 
                   y = fitted, 
                   xend = RRI, 
                   yend = RCI, 
                   color = residual_sign),
               linetype = "dashed", alpha = 0.7, size = 0.9) +
  
  #Data points colored by residual sign
  geom_point(aes(color = residual_sign), size = 4, alpha = 0.8) +
  
  #Fitted points on the line (showing the linear model)
  geom_point(aes(x = RRI, y = fitted), 
             color = "black", 
             size = 2, 
             shape = 21, 
             fill = "white") +
  
  scale_color_manual(values = c("Positive" = "#E31A1C", "Negative" = "#1F78B4"),
                     name = "Residual Sign") +
  labs(
    x = "RRI (Mean per Subject)",
    y = "RCI (Mean per Subject)"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.85),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    aspect.ratio = 1,
    axis.text = element_text(angle = 0, hjust = 1, size = 12),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))
#Dashed lines show residuals from linear fit. Consecutive points of same 
#color form 'runs'. Fewer runs than expected indicates systematic 
#clustering (non-linearity).

print(runs_plot)
#Recommended dimeensions 800x800

#Now, you may proceed with script number 3