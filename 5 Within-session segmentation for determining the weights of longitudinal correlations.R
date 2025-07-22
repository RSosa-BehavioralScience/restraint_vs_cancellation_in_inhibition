#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('boot')) install.packages('boot'); library(boot)
if (!require('rptR')) install.packages('rptR'); library(rptR)

#Prepare this column for numerical analysis
trials$session <- as.numeric(sub("S", "", trials$session))

#remove unused sessions from the database
trials85dB <- trials %>% 
  filter(condition=="dB85"& !(session==15 | session==20))

#Add an additional column in the main database indicating whether the data from the current trial
#...pertains to the first (trials 1–16) or the second half (17–32 trials) of the session.
trials85dB <- trials85dB %>% 
  mutate(half = ifelse(trial < 17, 1, 2))

#Calculate the response restraint index for halves (rather than for session, as done previously)
RRI85 <- trials85dB %>% 
  group_by(rat, session, half) %>% 
  summarize(e_t_w_r = length(which(trial_type=="exc" & response_type=="w response")),
            i_t_w_r = length(which(trial_type=="inh" & response_type=="w response")),
            tot_e_t = length(which(trial_type=="exc" & response_type!="already in")),
            tot_i_t = length(which(trial_type=="inh" & response_type!="already in")),
            prop_etwr = (e_t_w_r+0.5)/(tot_e_t+1.0),
            prop_itwr = (i_t_w_r+0.5)/(tot_i_t+1.0),
            probit_e = qnorm(prop_etwr),
            probit_i = qnorm(prop_itwr),
            RRI = probit_e - probit_i)

#Calculate the response cancellation index for halves (rather than for session, as done previously)
RCI85 <- trials85dB %>% 
  group_by(rat, session, half) %>% 
  summarize(dur_q_e = mean(duration_q[which(trial_type=="exc")], na.rm=TRUE),
            dur_q_i = mean(duration_q[which(trial_type=="inh")], na.rm=TRUE),
            RCI = log10(dur_q_e/dur_q_i))

RvsC <- as.data.frame(cbind(RRI85$rat, RRI85$session, RRI85$RRI, RCI85$RCI))

colnames(RvsC) <- c("rat", "session", "RRI", "RCI")

RvsC$session <- as.numeric(RvsC$session)

RvsC85_last <- RvsC %>% 
  filter(session > 13)

RvsC_halves <- cbind(RCI85, RRI85$RRI)
RvsC_halves <- RvsC_halves %>% select(-dur_q_e, -dur_q_i)
RvsC_halves <- RvsC_halves %>% filter(session > 13)
colnames(RvsC_halves)[5] <- "RRI"

#Create a data frame to store within session repeatability analyses
ws_rpt_RRI_par <- data.frame(mean_boot=numeric(), 
                             median_boot=numeric(),
                             LLCI_boot=numeric(), 
                             ULCI_boot=numeric(),
                             p_LRT=numeric(), 
                             mean_permut=numeric(), 
                             median_permut=numeric(), 
                             LLCI_permut=numeric(), 
                             ULCI_permut=numeric(), 
                             p_permut=numeric())

library(rptR)
set.seed(1401)

#Obtain within-session repeatabilities (intra-class correlation coefficients) by subject for the 
#response restraint index
for (i in 1:length(unique(RvsC_halves$rat))){
  icc_RRI_halves <- rpt(RRI ~ half + (1|session), grname=c("session"), data=RvsC_halves[which(RvsC_halves$rat==unique(RvsC_halves$rat)[i]), ],
                        nboot = 1000, npermut = 1000, datatype = "Gaussian")
  par <- c(mean(icc_RRI_halves$R_boot$session), 
           median(icc_RRI_halves$R_boot$session),
           quantile(icc_RRI_halves$R_boot$session, 0.025),
           quantile(icc_RRI_halves$R_boot$session, 0.975),
           icc_RRI_halves$P$LRT_P,
           mean(icc_RRI_halves$R_permut$session),
           median(icc_RRI_halves$R_permut$session),
           quantile(icc_RRI_halves$R_permut$session, 0.025),
           quantile(icc_RRI_halves$R_permut$session, 0.975),
           icc_RRI_halves$P$P_permut)
  ws_rpt_RRI_par <- rbind(ws_rpt_RRI_par, par)
  row.names(ws_rpt_RRI_par)[i] <- unique(RvsC_halves$rat)[i]
}

colnames(ws_rpt_RRI_par) <- c("mean_boot", "median_boot", "2.5_%_boot",
                              "97.5_%_boot", "p_LRT", "mean_permut",
                              "median_permut", "2.5_%_permut", "97.5_%_permut",
                              "p_permut")

ws_rpt_RCI_par <- data.frame(mean_boot=numeric(), 
                             median_boot=numeric(),
                             LLCI_boot=numeric(), 
                             ULCI_boot=numeric(),
                             p_LRT=numeric(), 
                             mean_permut=numeric(), 
                             median_permut=numeric(), 
                             LLCI_permut=numeric(), 
                             ULCI_permut=numeric(), 
                             p_permut=numeric())

#Set seed for reproducibility
set.seed(1401)

#Calculate within-session repeatabilities (i.e., intra-class correlations) by subject for the 
#response cancellation index subject. 1K iterations produce reasonably robust results.
for (i in 1:length(unique(RvsC_halves$rat))){
  icc_RCI_halves <- rpt(RCI ~ half + (1|session), grname=c("session"), data=RvsC_halves[which(RvsC_halves$rat==unique(RvsC_halves$rat)[i]), ],
                        nboot = 1000, npermut = 1000, datatype = "Gaussian")
  par <- c(mean(icc_RCI_halves$R_boot$session), 
           median(icc_RCI_halves$R_boot$session),
           quantile(icc_RCI_halves$R_boot$session, 0.025),
           quantile(icc_RCI_halves$R_boot$session, 0.975),
           icc_RCI_halves$P$LRT_P,
           mean(icc_RCI_halves$R_permut$session),
           median(icc_RCI_halves$R_permut$session),
           quantile(icc_RCI_halves$R_permut$session, 0.025),
           quantile(icc_RCI_halves$R_permut$session, 0.975),
           icc_RCI_halves$P$P_permut)
  ws_rpt_RCI_par <- rbind(ws_rpt_RCI_par, par)
  row.names(ws_rpt_RCI_par)[i] <- unique(RvsC_halves$rat)[i]
}

#Name columns appropriately
colnames(ws_rpt_RCI_par) <- c("mean_boot", "median_boot", "2.5_%_boot",
                              "97.5_%_boot", "p_LRT", "mean_permut",
                              "median_permut", "2.5_%_permut", "97.5_%_permut",
                              "p_permut")

#Rename data frame for later use.
adjusted <- ws_rpt_RCI_par

#Now, you may proceed with script number 6 or save the current evironment to continue later.