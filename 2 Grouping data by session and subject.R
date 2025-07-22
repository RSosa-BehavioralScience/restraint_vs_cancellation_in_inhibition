#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('boot')) install.packages('boot'); library(boot)

#Strip the alphabetic component from the 'session' column, then set its data type to numeric
trials$session <- as.numeric(sub("S", "", trials$session))

#Remove sessions spoiled due to thunderstorm-induced blackouts (see Data Analysis section in the manuscript)
trials85dB <- trials %>% 
  filter(condition=="dB85"& !(session==15 | session==20))

#RESPONSE RESTRAINT CALCULATION

#Generate response restraint metric by rat and session
RRI85 <- trials85dB %>% 
  group_by(rat, session) %>% 
  summarize(e_t_w_r = length(which(trial_type=="exc" & response_type=="w response")),
            i_t_w_r = length(which(trial_type=="inh" & response_type=="w response")),
            tot_e_t = length(which(trial_type=="exc" & response_type!="already in")),
            tot_i_t = length(which(trial_type=="inh" & response_type!="already in")),
            prop_etwr = (e_t_w_r+0.5)/(tot_e_t+1.0),
            prop_itwr = (i_t_w_r+0.5)/(tot_i_t+1.0),
            probit_e = qnorm(prop_etwr),
            probit_i = qnorm(prop_itwr),
            RRI = probit_e - probit_i)

#Prepare the function for integration into the bootstrapping routine later on
mean_fun <- function(data, indices) {
  return(mean(data[indices]))
}

#Implement a bootstrapping routine to estimate the uncertainty associated with the RRI
RRI_subj_means <- RRI85 %>% 
  group_by(session) %>% 
  summarize(x_bar_etwr = mean(prop_etwr),
            x_bar_itwr = mean(prop_itwr),
            x_bar_RRI = mean(RRI),
            lower_CI_e = quantile(((boot(data=prop_etwr, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_e = quantile(((boot(data=prop_etwr, statistic=mean_fun, R=1000))$t), 0.84),
            lower_CI_i = quantile(((boot(data=prop_itwr, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_i = quantile(((boot(data=prop_itwr, statistic=mean_fun, R=1000))$t), 0.84),
            lower_CI_RRI = quantile(((boot(data=RRI, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_RRI = quantile(((boot(data=RRI, statistic=mean_fun, R=1000))$t), 0.84))

#Transform the RRI data to plot it as a secondary dependient variable in conjunction with 
#proportion data 
#Prepare a linear rescaling function
rescale_to_range <- function(x, old_range, new_range) {
  ((x - old_range[1]) / (old_range[2] - old_range[1])) * (new_range[2] - new_range[1]) + new_range[1]
}
#Apply the transformation the RRI data to convert it to a proportion scale 
new_range <- c(min(RRI_subj_means$lower_CI_i), max(RRI_subj_means$upper_CI_e))
old_range <- c(min(RRI_subj_means$lower_CI_RRI), max(RRI_subj_means$upper_CI_RRI))
RRI_subj_means$RRI_transformed <- rescale_to_range(RRI_subj_means$x_bar_RRI, old_range, new_range)
RRI_subj_means$l_RRI_transformed <- rescale_to_range(RRI_subj_means$lower_CI_RRI, old_range, new_range)
RRI_subj_means$u_RRI_transformed <- rescale_to_range(RRI_subj_means$upper_CI_RRI, old_range, new_range)

#Number the usable sessions sequentially from one to twenty-two
RRI_subj_means$session <- c(1:22)

#Rearrange the data frame in long format for plotting purposes
RRI_subj_meanss <- rbind(cbind(RRI_subj_means$x_bar_etwr,
                               RRI_subj_means$lower_CI_e,
                               RRI_subj_means$upper_CI_e),
                         cbind(RRI_subj_means$x_bar_itwr,
                               RRI_subj_means$lower_CI_i,
                               RRI_subj_means$upper_CI_i),
                         cbind(RRI_subj_means$RRI_transformed,
                               RRI_subj_means$l_RRI_transformed,
                               RRI_subj_means$u_RRI_transformed))
RRI_subj_meanss <- as.data.frame(RRI_subj_meanss)
colnames(RRI_subj_meanss) <- c("x_bar", "lower_CI", "upper_CI")
RRI_subj_meanss$variable <- c(rep("etwr", 22), rep("itwr", 22), rep("RRI", 22))
RRI_subj_meanss$session <- c(1:22, 1:22, 1:22)

#Generate a plot for the left panel in Figure 3
pRRI <- ggplot(RRI_subj_meanss, aes(color=variable)) +
  geom_ribbon(data = subset(RRI_subj_meanss, variable=="RRI"),
              aes(x=session, ymin = lower_CI, ymax = upper_CI, fill = "RRI"), alpha = 0.55, linetype="blank") +
  geom_line(data = subset(RRI_subj_meanss, variable=="etwr"),
            aes(x=session, y = x_bar, color = "Excitatory_trials"), size=2) +
  geom_point(data = subset(RRI_subj_meanss, variable=="etwr"),
             aes(x=session, y = x_bar, color = "Excitatory_trials"), size=8) +
  geom_errorbar(data = subset(RRI_subj_meanss, variable=="etwr"),
                aes(x=session, ymin = lower_CI, ymax = upper_CI), color = "black", width = 0.4, size=2) +
  geom_line(data = subset(RRI_subj_meanss, variable=="itwr"),
            aes(x=session, y = x_bar, color = "Inhibitory_trials"), size=2) +
  geom_point(data = subset(RRI_subj_meanss, variable=="itwr"),
             aes(x=session, y = x_bar, color = "Inhibitory_trials"), size=8) +
  geom_errorbar(data = subset(RRI_subj_meanss, variable=="itwr"),
                aes(x=session, ymin = lower_CI, ymax = upper_CI), color = "gray60", width = 0.4, size=2) +
  scale_fill_manual(name="Right axis",values = c(RRI = "#5349FF"),
                    labels=c("Response\nrestraint index")) +
  scale_color_manual(name="Left axis", values = c(Excitatory_trials = "black", Inhibitory_trials = "gray60"),
                     labels=c("A+ trials", "AX– trails")) +
  scale_y_continuous(name = "Proportion of trials with at least one response", breaks = c(seq(.2,.9,.1)),
                     sec.axis = sec_axis(~ rescale_to_range(., new_range, old_range), name = "Response Restraint Index", 
                                         breaks = c(seq(-.5,2.5,.5)))) +
  scale_x_continuous(name = "Session", breaks = c(seq(1,22,1))) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.85, 0.5))
pRRI
#The recommended dimensions for the plot are 1200 x 900 pixels

###RESPONSE CANCELLATION CALCULATION

#Generate response restraint metric by rat and session
RCI85 <- trials85dB %>% 
  group_by(rat, session) %>% 
  summarize(dur_q_e = mean(duration_q[which(trial_type=="exc")], na.rm=TRUE),
            dur_q_i = mean(duration_q[which(trial_type=="inh")], na.rm=TRUE),
            RCI = log10(dur_q_e/dur_q_i))

#Prepare the function for integration into the bootstrapping routine later on
mean_fun <- function(data, indices) {
  return(mean(data[indices], na.rm=TRUE))
}

#Implement a bootstrapping routine to estimate the uncertainty associated with the RCI
RCI_subj_means <- RCI85 %>% 
  group_by(session) %>% 
  summarize(x_bar_dq_e = mean(dur_q_e, na.rm=TRUE),
            x_bar_dq_i = mean(dur_q_i, na.rm=TRUE),
            x_bar_RCI = mean(RCI, na.rm=TRUE),
            lower_CI_de = quantile(((boot(data=dur_q_e, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_de = quantile(((boot(data=dur_q_e, statistic=mean_fun, R=1000))$t), 0.84),
            lower_CI_di = quantile(((boot(data=dur_q_i, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_di = quantile(((boot(data=dur_q_i, statistic=mean_fun, R=1000))$t), 0.84),
            lower_CI_RCI = quantile(((boot(data=RCI, statistic=mean_fun, R=1000))$t), 0.16),
            upper_CI_RCI = quantile(((boot(data=RCI, statistic=mean_fun, R=1000))$t), 0.84))

#Transform the RCI data to plot it as a secondary dependient variable in conjunction with 
#duration data 
#Define a linear rescaling function
rescale_to_range <- function(x, old_range, new_range) {
  ((x - old_range[1]) / (old_range[2] - old_range[1])) * (new_range[2] - new_range[1]) + new_range[1]
}
#Apply the transformation the RRI data to convert it to a duration scale 
new_range <- c(min(RCI_subj_means$lower_CI_di), max(RCI_subj_means$upper_CI_de))
old_range <- c(min(RCI_subj_means$lower_CI_RCI), max(RCI_subj_means$upper_CI_RCI))
RCI_subj_means$RCI_transformed <- rescale_to_range(RCI_subj_means$x_bar_RCI, old_range, new_range)
RCI_subj_means$l_RCI_transformed <- rescale_to_range(RCI_subj_means$lower_CI_RCI, old_range, new_range)
RCI_subj_means$u_RCI_transformed <- rescale_to_range(RCI_subj_means$upper_CI_RCI, old_range, new_range)

#Number the usable sessions sequentially from one to twenty-two
RCI_subj_means$session <- c(1:22)

#RCI_subj_means_long <- reshape2::melt(RCI_subj_means, id.var = "session")

#Rearrange the data frame in long format for plotting purposes
RCI_subj_meanss <- rbind(cbind(RCI_subj_means$x_bar_dq_e,
                               RCI_subj_means$lower_CI_de,
                               RCI_subj_means$upper_CI_de),
                         cbind(RCI_subj_means$x_bar_dq_i,
                               RCI_subj_means$lower_CI_di,
                               RCI_subj_means$upper_CI_di),
                         cbind(RCI_subj_means$RCI_transformed,
                               RCI_subj_means$l_RCI_transformed,
                               RCI_subj_means$u_RCI_transformed))
RCI_subj_meanss <- as.data.frame(RCI_subj_meanss)
colnames(RCI_subj_meanss) <- c("x_bar", "lower_CI", "upper_CI")
RCI_subj_meanss$variable <- c(rep("dq_e", 22), rep("dq_i", 22), rep("RCI", 22))
RCI_subj_meanss$session <- c(1:22, 1:22, 1:22)

#Generate a plot for the right panel in Figure 3
pRCI <- ggplot(RCI_subj_meanss, aes(color=variable)) +
  geom_ribbon(data = subset(RCI_subj_meanss, variable=="RCI"),
              aes(x=session, ymin = lower_CI, ymax = upper_CI, fill = "RCI"), alpha = 0.5, linetype="blank") +
  geom_line(data = subset(RCI_subj_meanss, variable=="dq_e"),
            aes(x=session, y = x_bar, color = "Excitatory_trials"), size=2) +
  geom_point(data = subset(RCI_subj_meanss, variable=="dq_e"),
             aes(x=session, y = x_bar, color = "Excitatory_trials"), size=8) +
  geom_errorbar(data = subset(RCI_subj_meanss, variable=="dq_e"),
                aes(x=session, ymin = lower_CI, ymax = upper_CI), color = "black", width = 0.4, size=2) +
  geom_line(data = subset(RCI_subj_meanss, variable=="dq_i"),
            aes(x=session, y = x_bar, color = "Inhibitory_trials"), size=2) +
  geom_point(data = subset(RCI_subj_meanss, variable=="dq_i"),
             aes(x=session, y = x_bar, color = "Inhibitory_trials"), size=8) +
  geom_errorbar(data = subset(RCI_subj_meanss, variable=="dq_i"),
                aes(x=session, ymin = lower_CI, ymax = upper_CI), color = "gray60", width = 0.4, size=2) +
  scale_fill_manual(name="Right axis",values = c(RCI = "#ff4500"),
                    labels=c("Response\ncancellation index")) +
  scale_color_manual(name="Left axis", values = c(Excitatory_trials = "black", Inhibitory_trials = "gray60"),
                     labels=c("A+ trials", "AX– trails")) +
  scale_y_continuous(name = "Duration quotient", breaks = c(seq(.2,.9,.1)),
                     sec.axis = sec_axis(~ rescale_to_range(., new_range, old_range), name = "Response Cancellation Index", 
                                         breaks = c(seq(-.2,.9,.1)))) +
  scale_x_continuous(name = "Session", breaks = c(seq(1,22,1))) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.15, 0.80))
pRCI
#The recommended dimensions for the plot are 1200 x 900 pixels

#Generate a data frame containing both RRI and RCI data
RvsC <- as.data.frame(cbind(RRI85$rat, RRI85$session, RRI85$RRI, RCI85$RCI))
colnames(RvsC) <- c("rat", "session", "RRI", "RCI")
RvsC$session <- as.numeric(RvsC$session)

#Create a data frame that includes only the final 9 sessions identified as stable for later use
RvsC85_last <- RvsC %>% 
  filter(session > 13)

#Now, you may proceed with script 2.1

