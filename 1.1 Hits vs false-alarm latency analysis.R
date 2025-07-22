#Set the R console language to English to make troubleshooting and 
#help seeking more straightforward
Sys.setenv(LANG = "en")

#Check source data frame
trials

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('lme4')) install.packages('lme4'); library(lme4)
if (!require('emmeans')) install.packages('emmeans'); library(emmeans)
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)
if (!require('ggeffects')) install.packages('ggeffects'); library(ggeffects)

#Remove sessions spoiled due to thunderstorm-induced 
#blackouts (see Data Analysis section in the manuscript)
trials_original <- trials %>% 
  filter(condition=="dB85"& !(session==15 | session==20))

trials_original$session <- as.numeric(sub("S", "", trials_original$session))

#Filter session with stable performance
trials_original <- trials_original %>% 
  filter(!(session==15 | session==20)) %>% 
  filter(session > 13)
  
#Calculate means and confidence intervals for plotting
summary_lat_o <- trials_original %>%
  filter(!is.na(latency)) %>%
  group_by(rat, trial_type) %>%
  summarise(
    mean_latency = mean(latency),
    se = sd(latency) / sqrt(n()),
    ci_lower = mean_latency - 1.96 * se,
    ci_upper = mean_latency + 1.96 * se,
    .groups = 'drop'
  )

#Create x positions
summary_lat_o <- summary_lat_o %>%
  group_by(rat) %>%
  mutate(
    x_position = case_when(
      trial_type == "exc" ~ cur_group_id() - 0.2,
      trial_type == "inh" ~ cur_group_id() + 0.2
    )
  )

#Create individual comparisons  plot
ggplot(summary_lat_o, aes(x = x_position, y = mean_latency, color = trial_type)) +
  geom_point(size = 5) +
  geom_line(aes(group = rat), linewidth = 1, color = "gray") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, linewidth = 1) +
  scale_color_manual(values = c("exc" = "#ca82b8", "inh" = "#82ca9d"),
                     labels = c("Excitatory (A+)", "Inhibitory (AX–)")) +
  scale_x_continuous(breaks = 1:length(unique(summary_lat_o$rat)),
                     labels = unique(summary_lat_o$rat)) +
  labs(
    x = "Subject",
    y = "Mean RT +/– 95% confidence interval",
    color = "Trial Type"
  ) +
  theme_classic() +  
  theme(
    legend.position = c(0.65, 0.17),
    legend.background = element_rect(fill = alpha('white', 1), color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
    axis.title = element_text(size = 11),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  ) 

#recommended dimensions 1000x400

#Create the model
model <- lmer(latency ~ trial_type + (1|rat) + (1|session), 
              data = na.omit(trials_original))

#Get model summary
summary(model)

#Get estimated marginal means for trial types
emm <- emmeans(model, "trial_type")

#Get effect size (standardized beta coefficient)
standardize_parameters(model)

#Predicted values plot with confidence intervals

pred_plot <- ggpredict(model, terms = "trial_type")
pred_plot

pred_plot <- data.frame(
  trial_type = c("Excitatory", "Inhibitory"),
  Predicted = c(4.05, 2.95),
  CI_lower = c(3.77, 2.65),
  CI_upper = c(4.33, 3.24)
)

lat <- ggplot(pred_plot, aes(x = trial_type, y = Predicted, color = trial_type)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0, linewidth = 1.5) +
  scale_color_manual(values = c("#ca82b8", "#82ca9d")) +
  labs(
    #title = "Predicted Latency by Trial Type",
    x = "Trial Type",
    y = "Estimated RT +/– 95% confidence interval",
    color = "Trial Type"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 11),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(lat)
#Recomended dimensions 272 x 400

#Now, you may proceed with script 1.2