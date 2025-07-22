#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('rptR')) install.packages('rptR'); library(rptR)

############Simulated perfect correlations with observed ICCs

#Number of subjects and observations per subject
n_subjects <- 16
n_observations <- 9

#Observed intra-class correlations
rho_RRI <- 0.216 
rho_RCI <- 0.291 

#Create a data frame for the subjects and observations
data <- expand.grid(subject = 1:n_subjects,
                    observation = 1:n_observations)

#Create an empty vector for storing the resampled correlation coefficients
corr_distribution <- c()
#Setting seed for reproducibility
set.seed(1401)

for (i in 1:10000){
  
  #Generate subject effects
  subject_effects <- rnorm(n_subjects)
  
  #Add the *same* subject effect to RRI and RCI, scaled by the square root of their ICCs
  data$subject_effect_RRI <- subject_effects[data$subject] * sqrt(rho_RRI)
  data$subject_effect_RCI <- subject_effects[data$subject] * sqrt(rho_RCI)
  #Note that, at this point, subject averages are perfectly linearly correlated
  
  #Generate observation effects for RRI and RCI, scaled by the square root of the IICs complements
  data$observation_effect_RRI <- rnorm(n_subjects * n_observations) * sqrt(1 - rho_RRI)
  data$observation_effect_RCI <- rnorm(n_subjects * n_observations) * sqrt(1 - rho_RCI)
  
  #Combine subject and observation effects to create RRI and RCI
  data$RRI <- data$subject_effect_RRI + data$observation_effect_RRI
  data$RCI <- data$subject_effect_RCI + data$observation_effect_RCI
  
  #Calculate means per subject
  subject_means <- data %>%
    group_by(subject) %>%
    summarise(mean_RRI = mean(RRI), mean_RCI = mean(RCI))
  
  #Determine the correlation between subject-averaged RRI and RCI for a single simulation
  sample_corr <- cor(subject_means$mean_RRI, subject_means$mean_RCI)
  #Store the obtained correlation coefficient for assessing their aggregate performance 
  corr_distribution[i] <- sample_corr
}

#Obtain the median and confidence intervals of the distributions of correlation coefficients
quantile(corr_distribution, 0.5)
quantile(corr_distribution, 0.025)
quantile(corr_distribution, 0.975)

#Generate Figure 5
ggplot() +
  geom_density(aes(x = corr_distribution), binwidth = 0.01, fill = "skyblue", color = "black", alpha=0.4) +
  theme_classic() +
  geom_vline(aes(xintercept=quantile(corr_distribution, 0.5)), color="red", linetype="dotted", linewidth=2) +
  geom_vline(aes(xintercept=quantile(corr_distribution, 0.025)), color="gray", linewidth=1) +
  geom_vline(aes(xintercept=quantile(corr_distribution, 0.975)), color="gray", linewidth=1) +
  geom_vline(aes(xintercept=0.35), color="green", linewidth=1) +
  xlim(-1, 1) +
  xlab("Simulated Correlation Coefficients") +
  ylab("Density")

#A first step for validating this simulation is to change the values of 
#...intra-class correlation (both) to zero, in which case we observe a median
#...of zero. This is expected, because we cannot find a signal (in this case, the correlation) 
#...amidst absolute noise. Then, we may set both values at 1.0, in which case we observe a
#...median correlation of 1.0. This is expected inasmuch if we encounter no
#...variability within subjects' data, the perfect correlation that is set at
#...the beginning of the routine is retained. Another strategy may be 
#...modifying the sample size of subjects and sessions. If we set these values at 2, the lower 
#...confidence interval will be minus one; for this reason, we can conclude that with a smaller
#...sample space we would not been able to reject our null hipothesis. In contrast, if we multiply
#.. both the values of n_subjects and n_observations (i.e., number of sessions), we will have a
#...a lower confidence interval very close to 1.0. This suggests that obtaining the observed 
#...correlation coefficient with a larger sample size could have led to rejecting the null 
#...hypothesis (i.e., perfect linear correlation) with greater confidence.

#Next, we can replicate the procedure adding code for quantifying the intra-class
#...correlations of RRI and RCI at each resampling iteration.

#Even if the following verification manouvre is extremely computationally expensive,
#...it is crucial to validate that the simulation is akin to our rationale
#...of estimating the values of sampled correlation coefficients adding the 
#...precise amount of noise we observed provided that both indices are truly
#...perfectly linearly correlated.

#With the purpose of providing just a quick glance to this more complete
#...procedure, we set the number of resampling in the main iterative loop and 
#...in the embedded repeteability calculations at thirty. In order to perform a
#...more thorough analysis, increase the number of iterations in the 
#..."i in 1:30",  "nboot = 30", and "npermut = 30" arguments. Be aware that 
#...such an adjustement will make the duration overly long, but still.

#Empty the vector storing the distribution of correlation coefficients
corr_distribution <- c()
#Create two new empty vectors to store the intra-class correlation coefficients of both indices
RRI_ICC_dist <- c()
RCI_ICC_dist <- c()

set.seed(1401)

#Loop with intra-class correlation calculations added
for (i in 1:30){
  subject_effects <- rnorm(n_subjects)
  
  data$subject_effect_RRI <- subject_effects[data$subject] * sqrt(rho_RRI)
  data$subject_effect_RCI <- subject_effects[data$subject] * sqrt(rho_RCI)
  
  data$observation_effect_RRI <- rnorm(n_subjects * n_observations) * sqrt(1 - rho_RRI)
  data$observation_effect_RCI <- rnorm(n_subjects * n_observations) * sqrt(1 - rho_RCI)
  
  data$RRI <- data$subject_effect_RRI + data$observation_effect_RRI
  data$RCI <- data$subject_effect_RCI + data$observation_effect_RCI
  
  subject_means <- data %>%
    group_by(subject) %>%
    summarise(mean_RRI = mean(RRI), mean_RCI = mean(RCI))
  
  sample_corr <- cor(subject_means$mean_RRI, subject_means$mean_RCI)
  corr_distribution[i] <- sample_corr
  
  #Calculate the intra-class correlation for RRI in each iteration of the loop
  RRI_ICC <- rpt(RRI ~ (1 | subject), grname = "subject", data = data, 
                 nboot = 30, npermut = 30, datatype = "Gaussian")
  RRI_ICC_dist[i] <- RRI_ICC$R[[1]]
  
  #Calculate the intra-class correlation for RCI in each iteration of the loop
  RCI_ICC <- rpt(RCI ~ (1 | subject), grname = "subject", data = data, 
                 nboot = 30, npermut = 30, datatype = "Gaussian")
  RCI_ICC_dist[i] <- RCI_ICC$R[[1]]
}

#The median of the correlation coefficients should approximate that in Figure 5
hist(corr_distribution, breaks = 24)
(median_correlation <- median(corr_distribution))
(lower_bound <- quantile(corr_distribution, 0.025))
(upper_bound <- quantile(corr_distribution, 0.975))

#The median of the RRI repeatabilities should approximate to 0.216
hist(RRI_ICC_dist, breaks = 10)
(median_correlation <- median(RRI_ICC_dist))
(lower_bound <- quantile(RRI_ICC_dist, 0.025))
(upper_bound <- quantile(RRI_ICC_dist, 0.975))

#The median of the RCI repeatabilities should approximate to 0.291
hist(RCI_ICC_dist, breaks = 10)
(median_correlation <- median(RCI_ICC_dist))
(lower_bound <- quantile(RCI_ICC_dist, 0.025))
(upper_bound <- quantile(RCI_ICC_dist, 0.975))

#Now, you may proceed with script number 5 or save the current evironment to continue later.
