#Install or load required packages
if (!require('beepr')) install.packages('beepr'); library(beepr)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('rptR')) install.packages('rptR'); library(rptR)
if (!require('beepr')) install.packages('beepr'); library(beepr)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('TOSTER')) install.packages('TOSTER'); library(TOSTER)
if (!require('ggdensity')) install.packages('ggdensity'); library(ggdensity)
if (!require('lmerTest')) install.packages('lmerTest'); library(lmerTest)
if (!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)

##### CROSS-SECTIONAL CORRELATION OF SUBJECT AVERAGES

#For this analyses, we are going to use only the last nine sessions of the protocol
R_vs_C <- RvsC85_last
colnames(R_vs_C) <- c("ID", "Sesion", "RRI", "RCI")
R_vs_C$RRI <- as.numeric(R_vs_C$RRI)
R_vs_C$RCI <- as.numeric(R_vs_C$RCI)

#Calculate the average scores for each subject on the RCI and respective 95% confidence intervals
(xbar_RCI_85 <- R_vs_C %>%
  group_by(ID) %>% 
  summarise(x_bar_RCI = mean(RCI, na.rm =TRUE),
            u_RCI = mean(RCI, na.rm =TRUE)+(sd(RCI, na.rm=TRUE)/sqrt(length(which(!is.na(RCI)))))*2,
            l_RCI = mean(RCI, na.rm =TRUE)-(sd(RCI, na.rm=TRUE)/sqrt(length(which(!is.na(RCI)))))*2))

#Same for the RRI
(xbar_RRI_85 <- R_vs_C %>%
  group_by(ID) %>% 
  summarise(x_bar_RRI = mean(RRI, na.rm =TRUE),
            u_RRI = mean(RRI, na.rm =TRUE)+(sd(RRI, na.rm=TRUE)/sqrt(length(which(!is.na(RRI)))))*2,
            l_RRI = mean(RRI, na.rm =TRUE)-(sd(RRI, na.rm=TRUE)/sqrt(length(which(!is.na(RRI)))))*2))

#Merge average data frames
xbar_85 <- cbind(xbar_RCI_85[,1:2], xbar_RRI_85[,2])
#plot(xbar_85$x_bar_RRI, xbar_85$x_bar_RCI)


#Convert the raw values to z-scores
xbar_85 <- xbar_85 %>% 
  mutate(z_RCI= (x_bar_RCI-mean(x_bar_RCI))/sd(x_bar_RCI), 
         z_RRI=(x_bar_RRI-mean(x_bar_RRI))/sd(x_bar_RRI))

#Previsualization of the cross-sectional correlation
plot(xbar_RRI_85$x_bar_RRI, xbar_RCI_85$x_bar_RCI)
rl_RRIvsRCI85 <- lm(xbar_RCI_85$x_bar_RCI ~ xbar_RRI_85$x_bar_RRI)
abline(rl_RRIvsRCI85)

#Test whether the cross-sectional correlation coefficient is less than 1.0
bct <- boot_cor_test(xbar_RRI_85$x_bar_RRI, 
              xbar_RCI_85$x_bar_RCI, 
              alternative = "less", 
              method = "pearson", #Use Spearman method, because linearity was rejected
              alpha = 0.05,
              null=1,
              R=10000)
print(bct)
#Visualize the bootstrapped distribution of bootstrapped correlation coefficients
hist(bct$boot_res)
mean(bct$boot_res)
var(bct$boot_res)

#####DETRENDING BETWEEN-SESSION RESPONSE CANCELLATION DATA WITH POSSITIVE TRENDS

#Justification for the detrending operation:

#As we can see, fixed effects of Session are signifficant for RCI, but 
#...not for RRI

RRI_slope <- lmer(RRI ~ Sesion + (1|ID), data = R_vs_C)
summary(RRI_slope)

RCI_slope <- lmer(RCI ~ Sesion + (1|ID), data = R_vs_C)
summary(RCI_slope)

#Get standardized beta coefficient of the slope for the in-text report
standardize_parameters(RCI_slope)

#Now, for illustrative purposes, we first show how the detrending operation is conducted

#We can draw, for example, a single subject from the study
(subject <- R_vs_C %>% 
    filter(ID=="CDX"))

#Number sessions from one to nine
ses <- c(1:9)
#Create a vector containing RCI values from the selected subject
r_c_i <- subject$RCI
#Plot RCI versus Session as a time series
plot(ses, r_c_i)
#Conduct a linear regression with the time series 
rl_subject <- lm(r_c_i ~ ses)
#Generate a vector mapping expected RCI values at each Session according to the
#...regression
fit <- rl_subject$coefficients["(Intercept)"]+(ses*rl_subject$coefficients["ses"])
#Visualize this model fitting over the plot
lines(fit)
#Obtain the deviations from the fitted line to each data point
err <- r_c_i - fit
#Determine the average of observed RCI data
promedio_rci <- mean(r_c_i, na.rm = TRUE)
#Obtain the difference between the average and each deviation score
#This removes any trend in the time series, albeit preserving the variation 
#...between consecutive sessions
detrended_data <- promedio_rci + err
#Plot the detrended data
plot(detrended_data)
#For verification purposes, obtain the slope of the modifyed data series
rl_detrended <- lm(detrended_data ~ ses)
#Create a vector with data that fit the model
#The result is a virtually horizontal line
detrended_fit <- rl_detrended$coefficients["(Intercept)"]+(ses*rl_detrended$coefficients["ses"])
#Visualize the model over the detrended data, corroborating that
#...the sign and magnitude of deflections from the model are preserved
lines(detrended_fit)
#Verify that this detrending operation does not affect the mean
mean(r_c_i, na.rm = TRUE)
mean(detrended_data, na.rm = TRUE)

#We will perform the detrending operation whenever we found a subject
#...exhibiting a possitive trend, that is, when the RCI vs Session slope is
#...greater than zero. For example, in this case:
rl_subject$coefficients["ses"]

#Hence, the first step is to get the regression analysis for all subjects
#...and then to detrend selectively those cases with a possitive slope

#In order to do that, create a vector containing the names of all subjects
subjects <- levels(factor(as.factor(R_vs_C$ID)))

#Create an empty vector to store the slope values
slopes <- numeric(length = length(subjects))

#This routine obtains the slopes for each subject
for (i in 1:length(subjects)) {
  ses <- c(1:9)
  time_series <- R_vs_C %>% 
    #filter(dB=="85dB") %>%
    filter(ID==subjects[i])
  rl_timeseries <- lm(time_series$RCI ~ ses)
  slopes[i] <- rl_timeseries$coefficients["ses"]
}
#Print the values of the slopes
slopes
#Corroborate that the mean of the slopes is greater that zero
mean(slopes)  
#...and the standard error is relatively small
sd(slopes)
sd(slopes)/(sqrt(length(slopes))) 

#Create a vector containing the index of slopes greater that zero for
#...future reference
positive_slope_indexes <- which(slopes > 0)

#Create an empty data frame to store the modifyed data set including the 
#...detrended data
R_vs_C_85_detrended <- data.frame(ID=character(), Sesion=character(), RCI=double(), RRI=double())

#Remove the positive trend and store the data
for (i in 1:length(positive_slope_indexes)) {
  ses <- c(1:9)
  time_series <- R_vs_C %>% 
    filter(ID==subjects[positive_slope_indexes[i]])
  rl_timeseries <- lm(time_series$RCI ~ ses)
  fit <- rl_timeseries$coefficients["(Intercept)"]+(ses*rl_timeseries$coefficients["ses"])
  err <- time_series$RCI - fit
  promedio_rci <- mean(time_series$RCI, na.rm = TRUE)
  detrended_data <- promedio_rci + err
  time_series[ ,4] <- detrended_data 
  R_vs_C_85_detrended <- rbind(R_vs_C_85_detrended, time_series)
}

#Create a vector containing the indexes of the remaining subjects
non_positive_slope_indexes <- which(slopes <= 0)

#Add the unaltered data from the subjects with non-positive slopes
for (i in 1:length(non_positive_slope_indexes)) {
  ses <- c(1:9)
  time_series <- R_vs_C %>% 
    #filter(dB=="85dB") %>%
    filter(ID==subjects[non_positive_slope_indexes[i]])
  R_vs_C_85_detrended <- rbind(R_vs_C_85_detrended, time_series)
}

#Check the features of the newly created data frame
ncol(R_vs_C_85_detrended)
nrow(R_vs_C_85_detrended)

#Now that we have the detrended data base, we will proceed to use it for obtaining the longitudinal 
#correlation coefficients to assess the relationship between our behavioral indices over time

#Create empty vectors to fill them with longitudinal correlation coefficients for each subject...
#...and their corresponding variances
longitudinal_cors_85 <- numeric(length = length(subjects))
long_cor_variance_85 <- numeric(length = length(subjects))

#Fill the vectors with the calculated values
set.seed(1401)
for (i in 1:length(subjects)) {
  wdf <- R_vs_C_85_detrended %>% filter(ID==subjects[i]) 
  wdf <- na.omit(wdf)
  wl <- boot_cor_test(wdf$RCI, wdf$RRI, 
                      alternative = "two.sided", 
                      method = "pearson",
                      alpha = 0.05,
                      null=1,
                      R=1000)
  longitudinal_cors_85[i] <- wl$estimate
  long_cor_variance_85[i] <- var(wl$boot_res)
}

#Merge the vector into a new data frame we are going to use later
long_cors_plus_vars <- cbind(longitudinal_cors_85, long_cor_variance_85)

#Visualize the distribution of raw longitudinal correlations
hist(longitudinal_cors_85)

#Perform a one sided t-test to assess whether the aggregate correlation
#...coefficients differs from zero
t.test(longitudinal_cors_85)

#We will continue with the analysis of longitudinal correlation coefficients later

##### 
#INTRA-CLASS CORRELATION ANALYSES

#Use this seed for reproductability
set.seed(1401)

#Estimate the intra-class correlation for the response cancelation index
RCI_ICC_85dB <- rpt(RCI ~ (1 | ID), grname = "ID", data = R_vs_C_85_detrended, 
                    nboot = 10000, npermut = 10000, datatype = "Gaussian")

#This object contains the point estimate and uncertainty of the intra-class correlation
summary(RCI_ICC_85dB) #rpt = 0.29(CI 95 % 0.09-0.48, p = 0.000006) permut = 0.02 (CI 95 % 0-0.1, p = 0.0001)
bs_icc_RCI <- plot(RCI_ICC_85dB) #Pre-visualize bootstrapping results
p_icc_RCI <- plot(RCI_ICC_85dB, type = "permut") #Pre-visualize permutation results

set.seed(1401)
#Estimate the intra-class correlation for the response restraint index
RRI_ICC_85dB <- rpt(RRI ~ (1 | ID), grname = "ID", data = R_vs_C_85_detrended, 
                    nboot = 10000, npermut = 10000, datatype = "Gaussian")

#This object contains the point estimate and uncertainty of the intra-class correlation
summary(RRI_ICC_85dB) #rpt = 0.22(CI 95 % 0.05-0.39, p = 0.000125) permut = 0.02 (CI 95 % 0-0.1, p = 0.0001)
bs_icc_RRI <- plot(RRI_ICC_85dB)
p_icc_RRI <- plot(RRI_ICC_85dB, type = "permut")

#Fit the visualization of intra-class correlation analyses in a 2x2 matrix
par(mfrow = c(2, 2))
plot(RRI_ICC_85dB)
plot(RRI_ICC_85dB, type = "permut")
plot(RCI_ICC_85dB)
plot(RCI_ICC_85dB, type = "permut")
#Recommended dimensions 

#Return visualization panel to default settings
par(mfrow = c(1,1))

#Creating Figure 4
while (!is.null(dev.list())) dev.off()
graphics.off()
svg("matrix_plot.svg", width = 12, height = 10)
split.screen(c(2, 2))
screen(1) 
plot(RRI_ICC_85dB)
screen(2) 
plot(RRI_ICC_85dB, type = "permut")
screen(3) 
plot(RCI_ICC_85dB)
screen(4) 
plot(RCI_ICC_85dB, type = "permut")
close.screen(all = TRUE)
dev.off()

################################################################

#Create Figure 5

#Establish parameters for the ellipsoid based in the obtained correlation
correlation <- 0.35
mean_vec <- c(0, 0)
cov_matrix <- matrix(c(1, correlation, correlation, 1), nrow = 2)

#Obtain the remaining parameters for the ellipsoid to cover a certain proportion of the data
confidence_level <- 0.90
eigen_decomp <- eigen(cov_matrix)
rotation_matrix <- eigen_decomp$vectors
lengths <- sqrt(eigen_decomp$values * qchisq(confidence_level, 2))

#Prepare the ellipsoid for visualization
ellipse_points <- data.frame(x = numeric(), y = numeric())
angles <- seq(0, 2 * pi, length.out = 100)
for(angle in angles) {
  point <- c(lengths[1] * cos(angle), lengths[2] * sin(angle))
  rotated_point <- rotation_matrix %*% point + mean_vec
  ellipse_points <- rbind(ellipse_points, data.frame(x = rotated_point[1], y = rotated_point[2]))
}

csc_rri_85 <- ggplot(data = xbar_85, aes(x=z_RRI, y=z_RCI))+
  geom_point(color="red", size=6)+
  geom_path(data = ellipse_points, aes(x = x, y = y), color = "red") +
   geom_vline(aes(xintercept=mean(z_RRI)), color="black", linetype="dashed", linewidth=1.5) +
  geom_vline(aes(xintercept=mean(z_RRI)-sd(z_RRI)), color="black", linetype="dashed", linewidth=0.5) +
  geom_vline(aes(xintercept=mean(z_RRI)+sd(z_RRI)), color="black", linetype="dashed", linewidth=0.5) +
   geom_hline(aes(yintercept=mean(z_RCI)), color="black", linetype="dashed", linewidth=1.5) +
  geom_hline(aes(yintercept=mean(z_RCI)-sd(z_RRI)), color="black", linetype="dashed", linewidth=0.5) +
  geom_hline(aes(yintercept=mean(z_RCI)+sd(z_RRI)), color="black", linetype="dashed", linewidth=0.5) +
  scale_x_continuous(name = "Z-Score RRI", limits = c(-2.5, 2.5), breaks = c(seq(-2,2,1))) +
  scale_y_continuous(name = "Z-Score RCI", limits = c(-2.5, 2.5), breaks = c(seq(-2,2,1))) +
  #geom_smooth(method="lm", color="antiquewhite4", fill="white", se=T) +
  theme_classic()

csc_rri_85
#Recomended dimension is 700 x 700 pixels

#####

#Prepare data for dissociation analysis via individual uncertainty
rvsc85 <- merge(xbar_RRI_85, xbar_RCI_85)

#Convert subject means and confidence intervals to z-scores
z_rvsc85 <-
  rvsc85 %>%
  mutate(l_RCI= (l_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         u_RCI= (u_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         x_bar_RCI= (x_bar_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         l_RRI= (l_RRI-mean(x_bar_RRI))/sd(x_bar_RRI),
         u_RRI= (u_RRI-mean(x_bar_RRI))/sd(x_bar_RRI),
         x_bar_RRI=(x_bar_RRI-mean(x_bar_RRI))/sd(x_bar_RRI))

#Set colors for subjects' data highlighting the case of dissociation
z_rvsc85$color <- c("cornsilk4", "cornsilk4", "cornsilk4", "red",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4")

#Set transparency parameters for subjects' data
z_rvsc85$alphas <- c(0.4, 0.4, 0.4, 1,
                     0.4, 0.4, 0.4, 0.4,
                     0.4, 0.4, 0.4, 0.4,
                     0.4, 0.4, 0.4, 0.4)

copy_z_rvsc85 <- z_rvsc85
z_rvsc85 <- copy_z_rvsc85

#Create Figure 6
rvsc_ebd<-ggplot(z_rvsc85, aes(x=x_bar_RRI, y=x_bar_RCI)) + geom_point(color=z_rvsc85$color, alpha=z_rvsc85$alphas, size=6) + theme_classic() +
  geom_errorbar(aes(ymin=l_RCI, ymax=u_RCI), colour=z_rvsc85$color, width=1.2, alpha = z_rvsc85$alphas, linewidth=1)+ 
  geom_errorbarh(aes(xmax =u_RRI, xmin = l_RRI), color=z_rvsc85$color, height=1.2, alpha = z_rvsc85$alphas, linewidth=1) +
  scale_x_continuous(name = "Z Score RRI", limits = c(-3.5, 4), breaks = c(seq(-3,4,1))) +
  scale_y_continuous(name = "Z Score RCI", limits = c(-3.5, 4), breaks = c(seq(-3,4,1))) +
  geom_abline(slope = 1)
#Print Figure 6
rvsc_ebd
#Recommended dimension is 700 x 700 pixels

#Recall the aspect of the bivariate distribution of our observed data
plot(rvsc85$x_bar_RCI ~ rvsc85$x_bar_RRI)
abline(reg_85)

#Generate a linear regression between RRI and RCI
reg_85 <- lm(rvsc85$x_bar_RCI ~ rvsc85$x_bar_RRI)

#Create a new column to store the modify data
rvsc85$v_adjust <- vector("numeric", nrow(rvsc85))

#Modify data such that data points are aligned to the regression model
for (i in 1:nrow(rvsc85)){
  #rvsc85$v_adjust[i] <-
  rvsc85$v_adjust[i] <- 
    ((reg_85$coefficients[2]*rvsc85$x_bar_RRI[i])+reg_85$coefficients[1])-rvsc85$x_bar_RCI[i]
}

#Confirm that the data in the new column is now perfectly linearly correlated
plot((rvsc85$x_bar_RCI+rvsc85$v_adjust) ~ rvsc85$x_bar_RRI)
abline(reg_85)

#Prepare a data frame for forcing the perfect linear correlation into empirical data
v_adj_85 <- R_vs_C 
  
#Now, that we have the outline of how would the perfect linear correlation
#...apear, we can move session-subject data points such that their means match
#...with the newly modified data.
for (i in 1:nrow(v_adj_85)){ 
  if (is.na(v_adj_85$RCI[i])==FALSE){ 
    v_adj_85$RCI[i] <- 
      #When moving each point, we should multiply the deflection from the 
      #...diagonal by the observed correlation (0.3519) such as to preserve
      #...the aspect ratios of confidence intervals (hence, preserving the 
      #...intra-class correlations of both RRI and RCI)
      rvsc85$x_bar_RCI[which(rvsc85$ID==v_adj_85$ID[i])] + (v_adj_85$RCI[i] - rvsc85$x_bar_RCI[which(rvsc85$ID==v_adj_85$ID[i])])*0.3519
    v_adj_85$RCI[i] <- 
      v_adj_85$RCI[i] + rvsc85$v_adjust[which(rvsc85$ID==v_adj_85$ID[i])]
  } 
}

#Repeat data processing for summary statistics and standardization performed
#...with the unaltered data

(xbar_RCI_85 <- v_adj_85 %>%
    group_by(ID) %>% 
    summarise(x_bar_RCI = mean(RCI, na.rm =TRUE),
              u_RCI = mean(RCI, na.rm =TRUE)+(sd(RCI, na.rm=TRUE)/sqrt(length(which(!is.na(RCI)))))*2,
              l_RCI = mean(RCI, na.rm =TRUE)-(sd(RCI, na.rm=TRUE)/sqrt(length(which(!is.na(RCI)))))*2))

(xbar_RRI_85 <- v_adj_85 %>%
    group_by(ID) %>% 
    summarise(x_bar_RRI = mean(RRI, na.rm =TRUE),
              u_RRI = mean(RRI, na.rm =TRUE)+(sd(RRI, na.rm=TRUE)/sqrt(length(which(!is.na(RRI)))))*2,
              l_RRI = mean(RRI, na.rm =TRUE)-(sd(RRI, na.rm=TRUE)/sqrt(length(which(!is.na(RRI)))))*2))

xbar_85 <- cbind(xbar_RCI_85[,1:2], xbar_RRI_85[,2])

xbar_85 <- xbar_85 %>% 
  mutate(z_RCI= (x_bar_RCI-mean(x_bar_RCI))/sd(x_bar_RCI), 
         z_RRI=(x_bar_RRI-mean(x_bar_RRI))/sd(x_bar_RRI))

#Corroborate perfect linear correlation
plot(xbar_RRI_85$x_bar_RRI, xbar_RCI_85$x_bar_RCI)
rl_RRIvsRCI85 <- lm(xbar_RCI_85$x_bar_RCI ~ xbar_RRI_85$x_bar_RRI)
abline(rl_RRIvsRCI85)
cor.test(xbar_RRI_85$x_bar_RRI, xbar_RCI_85$x_bar_RCI)#, use = "pearson")

#visualizing perfect correlation

rvsc85 <- merge(xbar_RRI_85, xbar_RCI_85)

z_rvsc85 <-
  rvsc85 %>% 
  mutate(l_RCI= (l_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         u_RCI= (u_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         x_bar_RCI= (x_bar_RCI-mean(x_bar_RCI))/sd(x_bar_RCI),
         l_RRI= (l_RRI-mean(x_bar_RRI))/sd(x_bar_RRI),
         u_RRI= (u_RRI-mean(x_bar_RRI))/sd(x_bar_RRI),
         x_bar_RRI=(x_bar_RRI-mean(x_bar_RRI))/sd(x_bar_RRI))

z_rvsc85$color <- c("cornsilk4", "cornsilk4", "cornsilk4", "red",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4",
                    "cornsilk4", "cornsilk4", "cornsilk4", "cornsilk4")

z_rvsc85$alphas <- c(0.4, 0.4, 0.4, 1,
                     0.4, 0.4, 0.4, 0.4,
                     0.4, 0.4, 0.4, 0.4,
                     0.4, 0.4, 0.4, 0.4)

copy_z_rvsc85 <- z_rvsc85
z_rvsc85 <- copy_z_rvsc85

#Create left panel of Figure 7
rvsc_ebd<-ggplot(z_rvsc85, aes(x=x_bar_RRI, y=x_bar_RCI)) + geom_point(color=z_rvsc85$color, alpha=z_rvsc85$alphas, size=6) + 
  theme_classic(base_size = 28) +
  geom_errorbar(aes(ymin=l_RCI, ymax=u_RCI), colour=z_rvsc85$color, width=1.2, alpha = z_rvsc85$alphas, size=1)+ 
  geom_errorbarh(aes(xmax =u_RRI, xmin = l_RRI), color=z_rvsc85$color, height=1.2, alpha = z_rvsc85$alphas, size=1) +
  scale_x_continuous(name = "Z Score RRI", limits = c(-4, 3), breaks = c(seq(-4,3,1))) +
  scale_y_continuous(name = "Z Score RCI", limits = c(-4, 3), breaks = c(seq(-4,3,1))) +
  geom_abline(slope = 1)
#Print plot
rvsc_ebd

#It is important to corroborate that the intra-class correlations are
#...(virtually) preserved after modifying the data to be perfectly linnearly 
#...correlated

#Repeatability analyses are set to conduct only 1000 iterations for expedite
#...performance, given that this is not a definitive test

set.seed(1401)
RCI_ICC_unaltered <- rpt(RCI ~ (1 | ID), grname = "ID", data = R_vs_C, 
                    nboot = 1000, npermut = 1000, datatype = "Gaussian")
summary(RCI_ICC_unaltered)

RRI_ICC_unaltered <- rpt(RRI ~ (1 | ID), grname = "ID", data = R_vs_C, 
                    nboot = 1000, npermut = 1000, datatype = "Gaussian")
summary(RRI_ICC_unaltered)

RCI_ICC_modifyied <- rpt(RCI ~ (1 | ID), grname = "ID", data = v_adj_85, 
                    nboot = 1000, npermut = 1000, datatype = "Gaussian")
summary(RCI_ICC_modifyied)

RRI_ICC_modifyied <- rpt(RRI ~ (1 | ID), grname = "ID", data = v_adj_85, 
                    nboot = 1000, npermut = 1000, datatype = "Gaussian")
summary(RRI_ICC_modifyied)

#####
#We are ready to take the modified (perfectly correlated) data to the resampling analysis

#We will first clone the modifyied data frame 
df <- v_adj_85

#Define function to resample observations, calculate means, and then correlate
cor_fun <- function(data, indices) {
  resampled_data <- data[indices,]
  
  #Compute mean of resampled data
  resampled_means <- resampled_data %>%
    group_by(ID) %>%
    summarise(RRI_mean = mean(RRI, na.rm = TRUE),
              RCI_mean = mean(RCI, na.rm = TRUE),
              .groups = 'drop')
  
  #Return correlation of means
  cor(resampled_means$RRI_mean, resampled_means$RCI_mean, use = "pairwise.complete.obs")
}

#Perform bootstrap sampling
set.seed(1401)  #for reproducibility
bootstrap_result <- boot(data = df, statistic = cor_fun, R = 10000)

#Extracting the bootstrapped correlations
correlation_distribution <- bootstrap_result$t

#Convert array to data frame for visualization
df_bootstrap <- data.frame(Correlation = correlation_distribution)

ci <- quantile(df_bootstrap$Correlation, c(0.025, 0.5, 0.975))
print(ci)

#Creating the right panel of Figure 7
ccr <- ggplot(df_bootstrap, aes(x = Correlation)) +
  geom_density(fill = "#008080", alpha = 0.4, binwidth = 0.01, color="#008080") +
  geom_vline(aes(xintercept=ci[[2]]), color="red", linetype="dotted", linewidth=2) +
  geom_vline(aes(xintercept=ci[[1]]), color="#606060", linewidth=1) +
  geom_vline(aes(xintercept=ci[[3]]), color="#606060", linewidth=1) +
  geom_vline(aes(xintercept=0.35), color="green", linewidth=1) +
  xlim(-1, 1) +
  labs(x = "Correlation Coefficient",
       y = "Density") +
  theme_classic(base_size = 28)
#Print plot
ccr

#Merge the recently created plots to constitute Figure 7
grid.arrange(rvsc_ebd, ccr, ncol=2)

#Save detrended data base

write.csv(R_vs_C_85_detrended,
          "C:\\Users\\Usuario\\Dropbox\\Publicaciones\\restraint vs cancellation\\R_vs_C_85_dtrended.csv")

#Now, you may proceed with script 3.1