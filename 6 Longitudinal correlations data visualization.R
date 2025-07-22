#Install or load required packages
if (!require('rptR')) install.packages('rptR'); library(rptR)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('GGally')) install.packages('GGally'); library(GGally)

#We will work with the detrended data base
rr_85dD_trim <- R_vs_C_85_detrended
  
  # read.csv(file.choose(), fileEncoding="UTF-8-BOM", header = TRUE) %>% 
  # filter(dB=="85dB")

#...alternatively, use the detrended data saved in R's environment 
rr_85dD_trim <- R_vs_C_85_detrended

#List subjects' names in this vector
rr_subjects <- unique(rr_85dD_trim$ID) 

#Filter the bivariate distribution of session data from RRI and RCI for the first subject of the 
#...list
lc_s1 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[1]) 

#Generate a scatter plot with a regression line and uncertainty of the relationship between both 
#...indices for this subject
plc_1 <- ggplot(data=lc_s1, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_1

#Do the same for the second subject of the list, and so on...
lc_s2 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[2])

plc_2 <- ggplot(data=lc_s2, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_2

lc_s3 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[3])

plc_3 <- ggplot(data=lc_s3, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_3

lc_s4 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[4])

plc_4 <- ggplot(data=lc_s4, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_4

lc_s5 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[5])

plc_5 <- ggplot(data=lc_s5, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_5

lc_s6 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[6])

plc_6 <- ggplot(data=lc_s6, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_6

lc_s7 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[7])

plc_7 <- ggplot(data=lc_s7, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_7

lc_s8 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[8])

plc_8 <- ggplot(data=lc_s8, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_8

lc_s9 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[9])

plc_9 <- ggplot(data=lc_s9, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_9

lc_s10 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[10])

plc_10 <- ggplot(data=lc_s10, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_10

lc_s11 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[11])

plc_11 <- ggplot(data=lc_s11, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_11

lc_s12 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[12])

plc_12 <- ggplot(data=lc_s12, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_12

lc_s13 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[13])

plc_13 <- ggplot(data=lc_s13, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_13

lc_s14 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[14])

plc_14 <- ggplot(data=lc_s14, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_14

lc_s15 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[15])

plc_15 <- ggplot(data=lc_s15, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_15

lc_s16 <- rr_85dD_trim %>% 
  filter(ID==rr_subjects[16])

plc_16 <- ggplot(data=lc_s16, mapping = aes(x=RCI,y=RRI)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=2.5, size=3, color="maroon", alpha = 0.7) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")
plc_16

#Generate a list countaining all the plots created above
rr_lc_plots <- list(plc_1, plc_2, plc_3, plc_4,
                    plc_5, plc_6, plc_7, plc_8,
                    plc_9, plc_10, plc_11, plc_12,
                    plc_13, plc_14, plc_15, plc_16)


#Pool all the plots in a 4x4 ggmatrix object with speciffic parameters
rr_lc <- ggmatrix(rr_lc_plots, 4, 4,
                  xlab = "Response Cancellation Index", ylab = "Response Restraint Index")
rr_lc <- rr_lc + theme(axis.title = element_text(size = 18),
              axis.ticks = element_blank(),
              axis.text = element_blank()) 

#Create a vector to store the longitudinal correlation coefficients for each subject
lcc <- vector("numeric", 16)

#Store the sixteen correlation coefficients in the newly created vector
for (i in 1:length(rr_subjects)){
  target <- rr_85dD_trim %>% filter(ID==rr_subjects[i])
  lcc[i] <- cor.test(target$RCI, target$RRI)$estimate
}

#Without the pooling strategy, the mean of the longitudinal correlation coefficients is notably low
#...and does not differ significantly from zero
lcc <- as.data.frame(lcc)
t.test(lcc$lcc)

#This beauty shows the case (plot not included in the manuscript)
lcc_p <- ggplot(data = lcc, mapping = aes(x=lcc)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA)) +
  labs(x="Correlation Coefficient", y="Frequency") +
  geom_histogram(binwidth = 0.16, color="maroon", fill="maroon", alpha=0.7, linewidth=1.5) +
  geom_vline(aes(xintercept=0.123), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=-0.106), color="antiquewhite4", linetype="dashed", linewidth=2) +
  geom_vline(aes(xintercept=0.353), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1, 1) #+
  #ylim(0, 14) +
  #annotate("text", label = target_signif_lab, x = -.95, y = 13, color = "antiquewhite4", size = 10, fontface = "bold")
lcc_p

#Now, you may proceed with script number 7 or save the current evironment to continue later.

