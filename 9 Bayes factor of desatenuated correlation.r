#Implementation of Matzke et al. (2017) Method following their WinBUGS model structure

if (!require('brms')) install.packages('brms'); library(brms)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('bayestestR')) install.packages('bayestestR'); library(bayestestR)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('rstan')) install.packages('rstan'); library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 3)

#=============================================================================
#Load Data and Get Individual Estimates
#=============================================================================

#Load data
data <- R_vs_C_85_detrended
colnames(data) <- c("rat", "session", "RRI_obs", "RCI_obs")

data <- data %>%
  mutate(
    rat = factor(rat),
    session = factor(session)
  )

nrow(data)
length(unique(data$rat))
length(unique(data$session))

#Get individual parameter estimates with uncertainty for each subject

(n_rats <- length(unique(data$rat)))
individual_estimates <- tibble()

#Set progress bar for intensive process
pb <- txtProgressBar(min = 0, max = n_rats, style = 3)

set.seed(1401)

#Fit a Bayesian model for each rat to get posterior distributions
for(i in 1:n_rats) {
  current_rat <- unique(data$rat)[i]
  rat_data <- data %>% filter(rat == current_rat)
  
  if(sum(!is.na(rat_data$RRI_obs)) < 2 || sum(!is.na(rat_data$RCI_obs)) < 2) {
    setTxtProgressBar(pb, i)
    next
  }
  
  #Fit individual Bayesian models for current rat
  tryCatch({
    #RRI model for current rat
    rri_model <- brm(
      RRI_obs ~ 1,
      data = rat_data,
      family = gaussian(),
      prior = c(
        prior(normal(0, 2), class = Intercept),
        prior(exponential(1), class = sigma)
      ),
      chains = 2,
      iter = 2000,
      warmup = 1000,
      cores = 1,
      silent = 2,
      refresh = 0
    )
    
    #RCI model for this current  
    rci_model <- brm(
      RCI_obs ~ 1,
      data = rat_data,
      family = gaussian(),
      prior = c(
        prior(normal(0, 2), class = Intercept),
        prior(exponential(1), class = sigma)
      ),
      chains = 2,
      iter = 2000,
      warmup = 1000,
      cores = 1,
      silent = 2,
      refresh = 0
    )
    
    #Extract posterior means and variances
    rri_posterior <- as_draws_df(rri_model)$Intercept
    rci_posterior <- as_draws_df(rci_model)$Intercept
    
    #Store posterior statistics
    rat_estimates <- tibble(
      rat = current_rat,
      RRI_mean = mean(rri_posterior),     
      RCI_mean = mean(rci_posterior),     
      RRI_se = sd(rri_posterior),         
      RCI_se = sd(rci_posterior),         
      RRI_n = sum(!is.na(rat_data$RRI_obs)),
      RCI_n = sum(!is.na(rat_data$RCI_obs))
    )
    
    individual_estimates <- bind_rows(individual_estimates, rat_estimates)
    
  }, error = function(e) {
  })
  
  setTxtProgressBar(pb, i)
}
close(pb)

individual_estimates <- individual_estimates %>%
  filter(!is.na(RRI_mean), !is.na(RCI_mean), RRI_se > 0, RCI_se > 0)

nrow(individual_estimates)

#Calculate the uncorrected (naïve) correlation
(naive_correlation <- cor(individual_estimates$RRI_mean, individual_estimates$RCI_mean))

#=============================================================================
#Hierarchical Model for Disattenuated Correlation
#=============================================================================

#WinBUGS model implemented in Stan
#...based on model_file_correlation.txt in Matzke et al. (2017)

#Prepare data for Stan
observed_matrix <- as.matrix(individual_estimates[, c("RRI_mean", "RCI_mean")])
sigma_epsilon_matrix <- as.matrix(individual_estimates[, c("RRI_se", "RCI_se")])

#Convert to precision (inverse variance)
Isigma_epsilon <- 1 / (sigma_epsilon_matrix^2)

N_rats <- nrow(observed_matrix)

#Stan model (exact translation of their WinBUGS model)
stan_model_code <- "
data {
  int<lower=0> N;
  matrix[N, 2] observed;
  matrix[N, 2] Isigma_epsilon;
  real prec_mu_theta;
  real prec_mu_beta;
  real b_sigma_theta;
  real b_sigma_beta;
}

parameters {
  matrix[N, 2] eta;
  vector[2] mu;
  real<lower=0> sigma_theta;
  real<lower=0> sigma_beta;
  real<lower=-1, upper=1> rho;
}

transformed parameters {
  matrix[2, 2] Sigma_cov;
  matrix[2, 2] ISigma_cov;
  
  // Build covariance matrix
  Sigma_cov[1, 1] = pow(sigma_theta, 2);
  Sigma_cov[1, 2] = rho * sigma_theta * sigma_beta;
  Sigma_cov[2, 1] = rho * sigma_theta * sigma_beta;
  Sigma_cov[2, 2] = pow(sigma_beta, 2);
  
  // Inverse covariance matrix
  ISigma_cov = inverse(Sigma_cov);
}

model {
  // Priors
  mu[1] ~ normal(0, sqrt(1/prec_mu_theta));
  mu[2] ~ normal(0, sqrt(1/prec_mu_beta));
  sigma_theta ~ uniform(0, b_sigma_theta);
  sigma_beta ~ uniform(0, b_sigma_beta);
  rho ~ uniform(-1, 1);
  
  // Likelihood
  for (i in 1:N) {
    // True values follow bivariate normal
    eta[i,] ~ multi_normal(mu, Sigma_cov);
    
    // WinBUGS: dnorm(mean, precision) where precision = 1/variance
    // Stan: normal(mean, sigma) where sigma = sqrt(variance) = sqrt(1/precision)
    for (j in 1:2) {
      observed[i,j] ~ normal(eta[i,j], sqrt(1/Isigma_epsilon[i,j]));
    }
  }
}
"

#Parametrization
stan_data <- list(
  N = N_rats,
  observed = observed_matrix,
  Isigma_epsilon = Isigma_epsilon,
  prec_mu_theta = 0.001,  
  prec_mu_beta = 0.001,     
  b_sigma_theta = 100,    
  b_sigma_beta = 100      
)

#Fit the Stan model
matzke_stan_model <- stan(
  model_code = stan_model_code,
  data = stan_data,
  seed = 1401,
  chains = 3,
  iter = 8000,  
  warmup = 3000,
  thin = 2,
  cores = 3,
  control = list(adapt_delta = 0.999, max_treedepth = 20, stepsize = 0.01),
  verbose = FALSE
)

print(matzke_stan_model)
pairs(matzke_stan_model, pars = c("rho", "sigma_theta", "sigma_beta"))

#Extract the correlation parameter (rho)
stan_samples <- extract(matzke_stan_model)
rho_samples <- stan_samples$rho

(rho_summary <- tibble(
  mean = mean(rho_samples),
  median = median(rho_samples),
  ci_lower = quantile(rho_samples, 0.025),
  ci_upper = quantile(rho_samples, 0.975),
  sd = sd(rho_samples)
))

#Check convergence
stan_summary <- summary(matzke_stan_model)$summary
(rho_rhat <- stan_summary[rownames(stan_summary) == "rho", "Rhat"])
(rho_neff <- stan_summary[rownames(stan_summary) == "rho", "n_eff"])

(max_rhat <- max(stan_summary[, "Rhat"], na.rm = TRUE))
(min_ess <- min(stan_summary[, "n_eff"], na.rm = TRUE))

#=============================================================================
#Quantifying strength of evidence
#=============================================================================

posterior_density <- density(rho_samples)

#Savage-Dickey Bayes factor visualization
#BF uses densities to test H0: ρ = 1

#Uncorrected (naive) correlation for reference
observed_correlation <- naive_correlation

#Prior density over defined range (uniform on [-1,1])
prior_density_value <- 0.5

#Find posterior density at ρ = 1
density_at_1 <- approx(posterior_density$x, posterior_density$y, xout = 1.0)$y

#Calculate Bayes factors
(BF_01 <- density_at_1 / prior_density_value)
(BF_10 <- 1 / BF_01)

#Create data for plotting - limit to [-1, 1]
plot_data <- tibble(
  rho = posterior_density$x,
  posterior = posterior_density$y,
  prior = prior_density_value  # Constant for uniform prior
) %>%
  filter(rho >= -1 & rho <= 1)

#Create Figure 10
p <- ggplot(plot_data, aes(x = rho)) +
  
  geom_segment(aes(x = -1, xend = -1, y = 0, yend = 0.5), color = "#4ecdc4", size = 1.5, linetype = "dashed") +
  geom_segment(aes(x = -1, xend = 1, y = 0.5, yend = 0.5), color = "#4ecdc4", size = 1.5, linetype = "dashed") +
  
  geom_line(aes(y = posterior), color = "#ff6b6b", size = 1.5, alpha = 0.9) +
  geom_area(aes(y = posterior), fill = "#ff6b6b", alpha = 0.2) +
  
  geom_vline(xintercept = 1.0, color = "black", size = 2) +
  geom_vline(xintercept = 1.0, color = "#ffe66d", size = 2, linetype = "dotted") +
  
  geom_vline(xintercept = observed_correlation, color = "darkgreen", size = 1.5, linetype = "solid") +
  
  geom_point(aes(x = 1.0, y = density_at_1), color = "#ff6b6b", size = 5) +
  geom_point(aes(x = 1.0, y = prior_density_value), color = "#4ecdc4", size = 5) +
  
  annotate("text", x = -0.4, y = max(plot_data$posterior) * 0.7, 
           label = "Posterior density\n  (disattenuated)", color = "#ff6b6b", size = 4, hjust = 0) +
  annotate("text", x = -0.8, y = prior_density_value + 0.05, 
           label = "Prior density (uniform)", color = "#4ecdc4", size = 4, hjust = 0) +
  annotate("text", x = 1.05, y = density_at_1 - 0.05, 
           label = paste("Posterior at ρ=1:\n", round(density_at_1, 3)), 
           color = "#ff6b6b", size = 3.5, hjust = 0) +
  annotate("text", x = 1.05, y = prior_density_value - 0.05, 
           label = paste("Prior at ρ=1:\n", round(prior_density_value, 3)), 
           color = "#4ecdc4", size = 3.5, hjust = 0) +
  annotate("text", x = observed_correlation - 0.1, y = max(plot_data$posterior) * 0.35,
           label = paste("Uncorrected ρ =", round(observed_correlation, 3)),
           color = "darkgreen", size = 4, hjust = 1, angle = 90) +
  
  annotate("text", x = -0.8, y = max(plot_data$posterior) * 0.9,
           label = paste("BF[01] =", round(BF_01, 3), "\nBF[10] =", round(BF_10, 3)),
           size = 4, hjust = 0, 
           bbox = list(boxtype = "round", facecolor = "white", alpha = 0.8)) +
  
  labs(
    x = "Correlation Coefficient",
    y = "Density") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  ) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1.5)) +
  ylim(0, max(plot_data$posterior) * 1.1)
p

#Recommended dimensions 700x700

###THE END
