##########################################################
# Name of file: 02b_QCovid_Agreement.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Calculates the concordance statistics after running
#                         02a_QCovid_Calculate_Scores.R
#                         02_QCovid_prediction_vectorised.R     
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
#library(survcomp)

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop


#z <- subset(df.risk.f, Qrisk_death >= 80)

#df2 <- df2 %>%  left_join(dplyr::select(z, -sex, -type))

#df2.sub <- slice_sample(df2, n=100000)

#Epi::ROC(test=df.risk.f$Qrisk_death, stat= df.risk.f$covid_cod, plot="ROC")

#z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ score, data=df2)
#concordance(z.fit)

z <- concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=filter(df.risk.f, hosp_remove==0),reverse=TRUE)
z.res.hf <- c(z$concordance, sqrt(z$var))
z <- concordance(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.f, reverse=TRUE)
z.res.df <- c(z$concordance, sqrt(z$var))
z <- concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=filter(df.risk.m, hosp_remove==0), reverse=TRUE)
z.res.hm <- c(z$concordance, sqrt(z$var))
z <- concordance(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.m, reverse=TRUE)
z.res.dm <- c(z$concordance, sqrt(z$var))
z.res <- rbind(z.res.hf, z.res.df, z.res.hm, z.res.dm)
z.res <- data.frame(Group=c("Hosp Female","Death_Female","Hosp_Male","Death_Male"),
                    Concordance=as.numeric(as.character(z.res[,1])), 
                    SE_C=as.numeric(as.character(z.res[,2])))
z.res <- z.res %>% mutate(Con_LCL=Concordance - 1.96*SE_C,Con_UCL=Concordance + 1.96*SE_C ) 

saveRDS(z.res, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_Concordance_",z.period,".rds"))




#z <- slice_sample(df.risk.f, n=10000)
#z <- df.risk.f[1:10000,]
#Hmisc::rcorrcens(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=z)
#concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=z,reverse=TRUE)


fun_royston <- function(z){
  royston_d <- z["D"]
  rd_lcl <- royston_d - 1.96* z["se(D)"]
  rd_ucl <- royston_d + 1.96* z["se(D)"]
  
  k = sqrt(8/pi)
  
  rsq <- (royston_d^2/k^2)/(pi^2/6 + royston_d^2/k^2)
  
  rsq_lci = (rd_lcl^2/k^2)/(pi^2/6 + rd_lcl^2/k^2)
  rsq_uci = (rd_lcl^2/k^2)/(pi^2/6 + rd_lcl^2/k^2)
  
  z_out <- data.frame(D=royston_d, D_LCL=rd_lcl, D_UCL=rd_ucl, R2=rsq, R2_LCL=rsq_lci, R2_UCL=rsq_uci)
  z_out
}

z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=filter(df.risk.f, hosp_remove==0), ties="breslow")
z.out <- royston(z.fit)
z.res.hf <- fun_royston(z.out)
z.fit <- coxph(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.f, ties="breslow")
z.out <- royston(z.fit)
z.res.df <- fun_royston(z.out)
z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=filter(df.risk.m, hosp_remove==0), ties="breslow")
z.out <- royston(z.fit)
z.res.hm <- fun_royston(z.out)
z.fit <- coxph(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.m, ties="breslow")
z.out <- royston(z.fit)
z.res.dm <- fun_royston(z.out)

z.res <- rbind(z.res.hf, z.res.df, z.res.hm, z.res.dm)
z.res <- cbind.data.frame(Group=c("Hosp Female","Death_Female","Hosp_Male","Death_Male"), z.res)

saveRDS(z.res, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_Royston_D_",z.period,".rds"))




#Hmisc::rcorr.cens(z$Qrisk_hosp, Surv(z$Time.To.Hosp, z$hosp_covid))
#Hmisc::rcorrcens(Surv(Time.To.Death, death_covid) ~ Qrisk_death, data=df.risk.f)
#Hmisc::somers2(df.risk.f$Qrisk_hosp, df.risk.f$hosp_covid)
#Hmisc::somers2(df.risk.f$Qrisk_death, df.risk.f$death_covid)


z.markers = list(Qrisk_hosp=df.risk.f$Qrisk_hosp)
z.b <- riskRegression::Score(z.markers,formula=Hist(Time.To.Hosp, hosp_covid)~1,
                      data=filter(df.risk.f, hosp_remove==0), metrics=c("Brier"),
                      summary="ibs",
                      times=max(df.risk.f$Time.To.Hosp))
z.res.hf <- z.b$Brier$score[2,c(3,5)]
z.markers = list(Qrisk_death=df.risk.f$Qrisk_death)
z.b <- riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=df.risk.f,metrics=c("Brier"),
                             summary="ibs",
                             times=max(df.risk.f$Time.To.Death))
z.res.df <- z.b$Brier$score[2,c(3,5)]

z.markers = list(Qrisk_hosp=df.risk.m$Qrisk_hosp)
z.b <- riskRegression::Score(z.markers,formula=Hist(Time.To.Hosp, hosp_covid)~1,
                             data=filter(df.risk.m, hosp_remove==0), metrics=c("Brier"),
                             summary="ibs",
                             times=max(df.risk.m$Time.To.Hosp))
z.res.hm <- z.b$Brier$score[2,c(3,5)]
z.markers = list(Qrisk_death=df.risk.m$Qrisk_death)
z.b <- riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=df.risk.m,metrics=c("Brier"),
                             summary="ibs",
                             times=max(df.risk.m$Time.To.Death))
z.res.dm <- z.b$Brier$score[2,c(3,5)]

z.res <- rbind(z.res.hf, z.res.df, z.res.hm, z.res.dm)
z.res <- cbind.data.frame(Group=c("Hosp Female","Death_Female","Hosp_Male","Death_Male"), z.res)

z.res <- z.res %>% mutate(lcl = Brier - 1.96*se.conservative, ucl = Brier + 1.96*se.conservative)

saveRDS(z.res, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_Brier_",z.period,".rds"))


#z <- slice_sample(df.risk.f, n=10000)
#z.markers = list(Qrisk_death=z$Qrisk_death)
#z.b <- riskRegression::Score(z.markers,formula=Hist(Time.To.Hosp, hosp_covid)~1,data=z,metrics=c("Brier"),
#                           summary="ibs",
#                           times=max(z$Time.To.Hosp))

#riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,metrics=c("AUC","Brier"),
#                      summary="ipa", times=max(z$Time.To.Death)-31)
#riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,metrics=c("AUC"),
#                      conf.int=TRUE, times=c(60,90))

#riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,summary="ibs",
#                      conf.int=TRUE, times=sort(unique(z$Time.To.Death)) )

#library(pec)
#z.markers = list(Qrisk_hosp=matrix(z$Qrisk_hosp, ncol=1))
#z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ Age + town + BMI, data=z, x=TRUE, y=TRUE)
#z1 <- pec::pec(z.fit, formula=Surv(Time.To.Hosp, hosp_covid)~1,data=z)
#plot(z1)
