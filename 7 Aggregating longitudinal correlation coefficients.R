#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('GGally')) install.packages('GGally'); library(GGally)
if (!require('grid')) install.packages('grid'); library(grid)

# First, we need to specify prior values for the correlation coefficient and the variance.
# Note that we've selected conservative priors, as our null hypothesis is supported at the start of 
#...the updating routine.

# Importantly, the prior for the correlation coefficient is expressed in terms of a Fisher-Z value.
# In this case, the value of 4 is equivalent to a coefficient of 0.9993293, 
#...which approximates 1.0 (our null hypothesis).
prior_cc <- 4
# The prior variance is quite high, meaning the estimate of the correlation coefficient will be 
#...heavily influenced by the data.
prior_v <- 10^8

#Prepare the data frame
long_cors_plus_vars <- as.data.frame(long_cors_plus_vars)

#Add the average of RRI and RCI intra-class correlations to the longitudinal correlation 
#...coefficients data frame to serve as a determinant of weights (numerator in icc to variance ratio)
for (i in 1:nrow(long_cors_plus_vars)){
  long_cors_plus_vars$mean_icc[i] <- mean(ws_rpt_RRI_par$mean_boot[i], ws_rpt_RCI_par$mean_boot[i])
}

#This loop implements the Zhang et al. routine to update the correlation coefficient by weighting 
#...the value for each individual based on their intra-class correlation to variance ratios
for (i in 1:nrow(long_cors_plus_vars)){
  z_cc <- 0.5 * log((1 + long_cors_plus_vars[i,1]) / (1 - long_cors_plus_vars[i,1]))
  posterior_cc <- 
    ((1/prior_v)*prior_cc + (long_cors_plus_vars[i,3]/long_cors_plus_vars[i,2])*z_cc)/((1/prior_v)+(long_cors_plus_vars[i,3]/long_cors_plus_vars[i,2]))
  posterior_v <- 1/((1/prior_v)+(long_cors_plus_vars[i,3]/long_cors_plus_vars[i,2]))
  prior_cc <- posterior_cc
  prior_v <- posterior_v
}

#Function to revert Fisher's Z-transformation
revert_fisher_z <- function(z) {
  return((exp(2 * z) - 1) / (exp(2 * z) + 1))
}

#This is the value of the pooled correlation coefficient
z_value = posterior_cc
original_correlation = revert_fisher_z(z_value)
print(paste("The original correlation coefficient is:", round(original_correlation, 4)))

#Obtaining the 95% confidence intervals of the pooled correlation coefficient
estimate_sd <- sqrt(posterior_v)
lower_CI <- original_correlation - (estimate_sd*1.96)
upper_CI <- original_correlation + (estimate_sd*1.96)

#Obtaining the individual weights for visualizing purposes
long_cors_plus_vars <- long_cors_plus_vars %>% 
  mutate(weight = mean_icc/long_cor_variance_85)

#We need to set a seed here for reproducibility of the beeswarm plot, as its creation involves 
#...a certain degree of randomness. This allows us to yield consistent results in subsequent runs.
set.seed(1401)
# Create a beeswarm plot using circular packing and gravitational effects to minimize 
#...overlapping of data points.
ind_and_glob_lc <- 
  ggplot(long_cors_plus_vars, aes(x=1, y = longitudinal_cors_85, size = weight, fill=long_cor_variance_85, color=mean_icc)) +
  scale_fill_viridis_b(name="Variance\n(fill)", option="rocket") +
  scale_color_viridis_b(name="Intra-Class\nCorrelation\n(border)", direction = -1, option = "rocket") +
  geom_point(shape=21, stroke=2, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.9) +
  scale_size_continuous(range = c(min(long_cors_plus_vars$weight), max(long_cors_plus_vars$weight)),
                        breaks = c(1,2,4,8,16)) +
  labs(x="", y = "Individual Longitudinal Correlation Coefficient") +
  theme_classic(base_size = 18) +
  theme(panel.background = element_rect(fill = "darkslategray"),
        panel.border = element_rect(fill=NA),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank(),
        legend.text= element_text(size = 10),
        legend.title = element_text(size = 12)) +
  geom_point(shape=18, x=1.6, y=original_correlation, color="aquamarine", size=6, show.legend = FALSE) +
  geom_errorbar(aes(x=1.6), ymin=lower_CI, ymax=upper_CI, size=0.8, color="aquamarine", width=.1, show.legend = FALSE) +
  guides(size=FALSE)+
  scale_y_continuous(breaks = seq(-0.5, 0.9, 0.1)) +
  scale_x_continuous(limits = c(0.5, 1.9))

ind_and_glob_lc

# Now, we need a somewhat convoluted (but effective) maneuver to combine a grid plot with 
#...a simple ggplot object to constitute Figure 9

#Print ggplot to a viewport
pushViewport(viewport(layout = grid.layout(1, 2)))
print(rr_lc, vp = viewport(layout.pos.col = 1, layout.pos.row = 1))

#Create a new page for ggmatrix and print it to a viewport
upViewport(0)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ind_and_glob_lc, vp = viewport(layout.pos.col = 2, layout.pos.row = 1))
#et voilà

#Recommended dimensions 1400x700

#Now, you may proceed with script number 8 or save the current evironment to continue later.


