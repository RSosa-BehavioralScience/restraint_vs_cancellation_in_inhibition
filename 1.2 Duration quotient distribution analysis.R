#Set the R console language to English to make troubleshooting and 
#help seeking more straightforward
Sys.setenv(LANG = "en")

#Load required packages
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('boot')) install.packages('boot'); library(boot)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)

#Remove sessions compromised by thunderstorm-induced blackouts and
#filter for dB85 condition and stable sessions (session 14 afterwards)
trials_original <- trials %>%
  filter(condition == "dB85") %>%
  mutate(session = as.numeric(sub("S", "", session))) %>%
  filter(session > 13 & !(session %in% c(15, 20)))

#Filter the data from inhibitory failures (or false alarms)
wrinh_data <- trials_original %>%
  filter(response_type == "w response", trial_type == "inh")

#This data frame still contains all the raw recorded events across  the 
#target sessions
events_original <- events %>%
  filter(condition == "dB85") %>%
  mutate(session = as.numeric(sub("S", "", session))) %>%
  filter(session > 13 & !(session %in% c(15, 20)))

#Generate a progress bar object to monitor the duration of 
#computationally intense routine ahead
pb <- txtProgressBar(min = 1,
                     max = nrow(trials_original),
                     style = 3,
                     width = 100,
                     char = "=")

#Remove irrelevant event
events_original <- events_original %>% 
  filter(!(label=="pre trial"))

#Create empty data frames to be filled later on
control_dq <- data.frame()  
pre_trial_int <- data.frame()

#Calculate the duration quotient for the control condition (ITI responses)
for (i in 1:nrow(trials_original)){
  setTxtProgressBar(pb, i)
  if (trials_original$trial_type[i] == "inh" & trials_original$response_type[i] == "w response"){
    latency <- trials_original$latency[i]
    boundary <- trials_original$duration_t[i]
    subject <- trials_original$rat[i]
    session <- trials_original$session[i]
    t_start <- trials_original$trial_beginning[i]
    iti <- trials_original$inter_trial_interval[i]
    sample_starts <- which(
      events_original$rat == subject &
        events_original$session == session &
        round(events_original$timestamp, 2) == round(t_start - iti, 2) &
        (endsWith(events_original$label, "trial begins") |  events_original$label == "session starts")
    )
    sample_ends <-
      which(events_original$rat == subject & 
              events_original$session == session & 
              round(events_original$timestamp, 2) == round(t_start, 2) &
              (endsWith(events_original$label, "trial begins") |  events_original$label == "session starts"))
     
    sample <- events_original[sample_starts:sample_ends, ] 
    
    sample <- sample[sample$timestamp >= sample$timestamp[1] + 16, ]
    
    for (j in 1:nrow(sample)){
      if (sample$label[j] == "entry"){
        k <- 1
        while (events_original$label[as.numeric(rownames(sample)[j])-k] != "exit" & events_original$label[as.numeric(rownames(sample)[j])-k] != "session starts"){
          k <- k + 1
        }
        if ((events_original$timestamp[as.numeric(rownames(sample)[j])] - events_original$timestamp[as.numeric(rownames(sample)[j])-k]) > latency){
          if ((events_original$timestamp[as.numeric(rownames(sample)[j])] + boundary) < events_original$timestamp[as.numeric(rownames(sample)[nrow(sample)])]){
            subsample <- sample[sample$timestamp < sample$timestamp[j] + boundary, ]
            subsample <- subsample[subsample$timestamp >= sample$timestamp[j], ]
            if (!any(subsample$label == "exit")) {
              dq_control <- 1
            } else {
              #Find the last occurrence of "exit" in subsample$label
              if (any(subsample$label == "exit")) {
                last_exit <- tail(subsample$timestamp[subsample$label == "exit"], 1)
              } else {
                last_exit <- NA  #Assign NA if there was no "exit"
              }
              
              #Compute dq_control
              dq_control <- (last_exit - subsample$timestamp[1]) / boundary
              
            }
            dq_data <- data.frame(subject = subject,
                                  session = session,
                                  trial = trials_original$specific_trial_no[i],
                                  dq = dq_control)
            control_dq <- rbind(control_dq, dq_data)
          }
        }
        }
      }
    }
  }

#Visualize the distribution of inhibitory-trial duration quotients
ggplot(wrinh_data, aes(x = duration_q)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Duration_q (Filtered)",
       x = "Duration_q",
       y = "Count") +
  theme_minimal()

#Visualize the distribution of control duration quotients
ggplot(control_dq, aes(x = dq)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Duration_q (Filtered)",
       x = "Duration_q",
       y = "Count") +
  theme_minimal()

#Combine the two data sets with an identifier
wrinh_data$source <- "wrinh_data"
control_dq$source <- "control_dq"
combined_data <- bind_rows(
  wrinh_data %>% select(duration_q, source) %>% rename(value = duration_q),
  control_dq %>% select(dq, source) %>% rename(value = dq)
)

#Plot overlapping density histograms
ggplot(combined_data, aes(x = value, fill = source)) +
  geom_histogram(aes(y = ..density..), position = "identity", bins = 30, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("wrinh_data" = "blue", "control_dq" = "brown"),
                    name = "Data Source",
                    labels = c("wrinh_data" = "Inhibitory failures", "control_dq" = "Inter-trial interval")) +
  labs(x = "Duration Quotient",
       y = "Density") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.80, 0.80),
    legend.background = element_rect(fill = alpha('white', 1), color = "black"),
    axis.text = element_text(angle = 0, hjust = 1, size = 10),
    axis.title = element_text(size = 13),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 13)
  ) 

#Dimensions 800 x 400

#Helper function to obtain beta density estimation
beta_kde <- function(data, x_vals, bandwidth = 5) {
  n <- length(data)
  density_estimate <- numeric(length(x_vals))
  
  #Compute density for each x value using weighted beta distributions
  for (i in seq_along(x_vals)) {
    density_estimate[i] <- mean(dbeta(x_vals[i], shape1 = data * bandwidth+1, shape2 = (1 - data) * bandwidth+1))
  }
  
  return(density_estimate)
}

#Define evaluation grid in the **original space** (0,1), ensuring full range coverage
x_vals_original <- seq(0.01, 0.99, length.out = 100)  # Avoid exact 0 and 1

#Compute beta KDE for both data sets separately
density_wrinh <- beta_kde(wrinh_data$duration_q, x_vals_original, bandwidth = 10)
density_control <- beta_kde(control_dq$dq, x_vals_original, bandwidth = 10)

#Compute density ratio
density_ratio <- density_wrinh / pmax(density_control, 1e-6)

#Plot density ratio
plot(x_vals_original, log10(density_ratio), type = "l", col = "blue", lwd = 2,
     main = "Beta Kernel Density Ratio",
     xlab = "Duration_q",
     ylab = "Log10 Density Ratio")
abline(h = 0, col = "red", lty = 2) #Reference line (null H) at log10(1) = 0
#Bimodality: supported

#Set number of bootstrap samples for obtaining the confidence interval
n_boot <- 1000 

#Helper function to compute the density ratio for bootstrapped samples
bootstrap_density_ratio <- function(data, indices, group) {
  #Resample data
  resampled_data <- data[indices]
  
  #Compute beta KDE for resampled data
  density_resampled <- beta_kde(resampled_data, x_vals_original, bandwidth = 10)
  
  return(density_resampled)
}

#Apply bootstrapping for confidence intervals

#Ensures reproducibility of uncertainty estimation
set.seed(1401)  
#Obtain bootstrapped density estimations separately for target and control values
boot_wrinh <- boot(data = wrinh_data$duration_q, statistic = bootstrap_density_ratio, R = n_boot, group = "wrinh")
boot_control <- boot(data = control_dq$dq, statistic = bootstrap_density_ratio, R = n_boot, group = "control")

#Compute density ratio for each bootstrap sample
density_ratio_boot <- boot_wrinh$t / pmax(boot_control$t, 1e-6)  

#Compute confidence intervals
ci_lower <- apply(density_ratio_boot, 2, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
ci_upper <- apply(density_ratio_boot, 2, function(x) quantile(x, probs = 0.975, na.rm = TRUE))
density_ratio_mean <- apply(density_ratio_boot, 2, mean, na.rm = TRUE)

#Create data frame for density ratio plot
plot_data <- data.frame(
  x = x_vals_original,
  mean_ratio = log10(density_ratio_mean),
  ci_lower = log10(ci_lower),
  ci_upper = log10(ci_upper)
)

#Create density ratio plot
density_plot <- ggplot(plot_data, aes(x = x)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              fill = "#d8ffb0", alpha = 0.8) +
  geom_line(aes(y = mean_ratio), color = "#215732", size = 2) +
  #Reference line at log10(1) = 0
  geom_hline(yintercept = 0, color = "#E71D36", linetype = "dashed", size = 1) +
  geom_line(aes(y = ci_lower), color = "#215732", linetype = "dashed", size = 0.8) +
  geom_line(aes(y = ci_upper), color = "#215732", linetype = "dashed", size = 0.8) +
  labs(
    x = "Duration Quotient",
    y = "Log10 Density Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              margin = margin(b = 20)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(density_plot)
#Recommended dimensions 800 x 400

#Ballistic process (AKA trigger failure) prevalence analysis

#Count occurrences of exact 1 in each data set separately
count_wrinh_ones <- sum(wrinh_data$duration_q == 1)
count_control_ones <- sum(control_dq$dq == 1)

#Determine total sample sizes
n_wrinh <- length(wrinh_data$duration_q)
n_control <- length(control_dq$dq)

#Print results
cat("Number of exact 1s in wrinh_data:", count_wrinh_ones, "\n")
cat("Number of exact 1s in control_dq:", count_control_ones, "\n")

#Construct a 2x2 contingency table
contingency_table <- matrix(c(count_wrinh_ones, n_wrinh - count_wrinh_ones,
                              count_control_ones, n_control - count_control_ones),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(Group = c("wrinh", "control"),
                                            Outcome = c("Exact_1", "Not_1")))

contingency_table

#Perform Chi-Squared Test
chi_sq_test <- chisq.test(contingency_table)

#Print results
print(chi_sq_test)

#Calculate odds for each group
odds_wrinh <- count_wrinh_ones / (n_wrinh - count_wrinh_ones)
odds_control <- count_control_ones / (n_control - count_control_ones)

#Compute odds ratio
odds_ratio <- odds_wrinh / odds_control

#Print the odds ratio
cat("Odds Ratio:", odds_ratio, "\n")

#Define contingency table values
a <- count_wrinh_ones
b <- n_wrinh - count_wrinh_ones
c <- count_control_ones
d <- n_control - count_control_ones

#Compute standard error of log(odds ratio)
se_log_or <- sqrt((1/a) + (1/b) + (1/c) + (1/d))

#Compute confidence intervals
log_or <- log(odds_ratio)
ci_lower <- exp(log_or - 1.96 * se_log_or)
ci_upper <- exp(log_or + 1.96 * se_log_or)

#Print the results
cat("95% Confidence Interval for Odds Ratio:", ci_lower, "-", ci_upper, "\n")

#Print contingency table
print(contingency_table)

#Print Chi-Squared test result
print(chi_sq_test)

#Print Odds Ratio and CI
cat("Odds Ratio:", odds_ratio, "\n")
cat("95% Confidence Interval for Odds Ratio:", ci_lower, "-", ci_upper, "\n")

#Now, you may proceed with script 2