##########################################################
# Name of file: 02b_QCovid_Calibration.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Calculates the Q-Covid score using Steven's functions in
#                         02_QCovid_prediction       
#                         
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#run 01_QCovid_Descriptions.R from EM to set up data frame (df)
#Libraries
library(plyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

#need df.risk.m, df.risk.f after running the scores
#z.period <-"P2" # second Period
z.df <- df.risk.f
z.sex<- "Female"
#z.df <- df.risk.m
#z.sex<- "Male"
z.b.death <- quantile(z.df$Qrisk_death, probs = seq(0,1,by=0.05))
z.df <- z.df %>% mutate(Risk_Death_20 = cut(Qrisk_death, breaks = z.b.death, include.lowest = TRUE,
                                            labels=paste0("Q",1:20)) )
z_dth <- z.df %>% group_by(Risk_Death_20) %>% 
  dplyr::summarise(N=n(), R = sum(death_covid), M=median(Qrisk_death))
z_dth <- z_dth %>% 
  mutate(P=R/N*100) %>% ungroup() %>% 
  dplyr::select(-N, -R) %>% 
  dplyr::rename(Pred_Risk = M, Obs_Risk=P) %>% 
  pivot_longer(cols=Pred_Risk:Obs_Risk)

if (z.period == "P1") z.title <- paste("Death - First Period - ",z.sex)
if (z.period == "P2") z.title <- paste("Death - Second Period - ",z.sex)
g1 <- z_dth %>% ggplot(aes(x=Risk_Death_20, y=value, colour=name)) + 
  geom_point() + labs(y="Risk (%)", x= "Vigentile Predicted Risk", colour= "",
                      title=z.title)

z.df <- filter(z.df, hosp_remove==0)
z.b.hosp <- quantile(z.df$Qrisk_hosp, probs = seq(0,1,by=0.05))
z.df <- z.df %>% mutate(Risk_Hosp_20 = cut(Qrisk_hosp, breaks = z.b.hosp, include.lowest = TRUE,
                                           labels=paste0("Q",1:20)) )
z_hosp <- z.df %>%  filter(hosp_remove==0) %>% group_by(Risk_Hosp_20) %>% 
  dplyr::summarise(N=n(), R = sum(hosp_covid), M=median(Qrisk_hosp))
z_hosp <- z_hosp %>% 
  mutate(P=R/N*100) %>% ungroup() %>% 
  dplyr::select(-N, -R) %>% 
  dplyr::rename(Pred_Risk = M, Obs_Risk=P) %>% 
  pivot_longer(cols=Pred_Risk:Obs_Risk)

z.title <- if (z.period == "P1") paste("Hospitalisation - First Period - ",z.sex)
z.title <- if (z.period == "P2") paste("Hospitalisation - Second Period - ",z.sex)
g2 <- z_hosp %>% ggplot(aes(x=Risk_Hosp_20, y=value, colour=name)) + 
  geom_point() + labs(y="Risk (%)", x= "Vigentile Predicted Risk", colour= "",
                      title=z.title)

saveRDS(g1, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Calibration_Curve_Death_",z.sex,"_",z.period,".rds"))
saveRDS(g2, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Calibration_Curve_Hosp_",z.sex,"_",z.period,".rds"))
