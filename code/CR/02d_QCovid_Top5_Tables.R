##########################################################
# Name of file: 02d_QCovid_Top5_Tables.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Calculates the descriptives for the top 5% for death
#                         Reads in the descriptives for the whole cohort
#                         needs df.risk.f, df.risk.m, df2 from 02b_Calibration.R
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
#library(janitor)

#Load data ----
Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop
##tables#################


#put the male/female back and join to df2
z.f <- df.risk.f %>% dplyr::select(EAVE_LINKNO, Qrisk_hosp, Qrisk_death)
z.m <- df.risk.m %>% dplyr::select(EAVE_LINKNO, Qrisk_hosp, Qrisk_death)
df2.risk <- bind_rows(z.f,z.m)  %>% right_join(df2, by="EAVE_LINKNO")

#calculate the upper 5%  and select these out
z <- quantile(df2.risk$Qrisk_death, 0.95)
df2.risk <- df2.risk %>% filter(Qrisk_death >=z)

#this bit is a quick fiddle to be abel to reuse some code
#rename df so as not to loose it
df.orig <- df
#assign the top 5 data to df
df <- df2.risk

#manually run 01c_QCovid_Description_Tables.R
#saving out the two files with _top5 as suffix

#read in the all population files and the top 5 combine and save out 
#firstly for co morbididites and then for demographics
ft <- readRDS(paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab1_clinical.rds"))
ft <- ft %>%  dplyr::select(Var, Values, N, Percent)
ft.t5 <- readRDS(paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab1_clinical_top5.rds"))
ft.t5 <- ft.t5 %>%  dplyr::select(Var, Values, N, Percent) %>% 
  dplyr::rename(N.T5 = N, P.T5 = Percent)

ft <- left_join(ft, ft.t5, by=c("Var","Values"))
ft <- ft %>% mutate(Row.P = N.T5/N*100)

write.csv(ft, "bmj_t5.csv", row.names=F)


ft <- readRDS(paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab2_demog.rds"))
ft.t5 <- readRDS(paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab2_demog_top5.rds"))
ft <- ft %>%  dplyr::select(Var, Values, N, Percent)
ft.t5 <- ft.t5 %>%  dplyr::select(Var, Values, N, Percent) %>% 
  dplyr::rename(N.T5 = N, P.T5 = Percent)

ft <- left_join(ft, ft.t5, by=c("Var","Values"))
ft <- ft %>% mutate(Row.P = N.T5/N*100)

write.csv(ft, "bmj_t5.csv", row.names=F)
