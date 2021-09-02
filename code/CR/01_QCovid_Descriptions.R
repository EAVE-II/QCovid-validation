##########################################################
# Name of file: 01_QCovid_Descriptions.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the Q Covid cohort and merges in the Endpoints 
#                         runs through tabulations and graphs
# Approximate run time: Unknown
##########################################################
#
#Superceded by Emily's version #############
#

# 01 Setup ####
#Libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
#Load data
#Location <- "/conf/"  # Server
Location <- "//isdsf00d03/"  # Desktop

QCovid_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/data/QCOVID.rds"))
QCovid_cohort <- filter(QCovid_cohort, !duplicated(EAVE_LINKNO)) %>% 
  dplyr::select(-DateOfBirth) 
QCovid_cohort <- QCovid_cohort %>%  filter(DeductionDate >= as.Date("2020-03-01") | is.na(DeductionDate))

QCovid_cohort <- slice_sample(QCovid_cohort, n=50000)

z.var.list <- names(QCovid_cohort)
z.var.list <- z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)]

QCovid_cohort <- QCovid_cohort %>% mutate_at(z.var.list, ~replace(., is.na(.), 0)) #replace missing values with zero
QCovid_cohort <- QCovid_cohort %>% mutate(Sex = case_when(Sex=="2" ~ "F",
                                                          TRUE~Sex))

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times.rds")) # 23 June endpoints
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))

#table(QCovid_cohort$EAVE_LINKNO %in% EAVE_cohort$EAVE_LINKNO)  #need tio check up on these ones

z <- EAVE_cohort %>% select(EAVE_LINKNO, SpecimenDate:Time.To.Hosp ) 
df <- QCovid_cohort %>% inner_join(z, by="EAVE_LINKNO")

EAVE_weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
df <- df %>%
  left_join(EAVE_weights, by="EAVE_LINKNO")



z.var <- "Q_BMI"
z.df <- select(df, starts_with(z.var)&ends_with(z.var), eave_weight, hosp_covid, death_covid) %>% rename(Var=1) %>% 
  group_by(Var) %>% summarise(N.Crude = n(), N=round(sum(eave_weight)), 
                                       hosp_covid=sum(hosp_covid) , death_covid=sum(death_covid)) %>% 
  mutate(p_hosp_covid = hosp_covid/N, p_death_covid = death_covid/N) %>% 
  mutate(r_hosp_covid = p_hosp_covid/first(p_hosp_covid), r_death_covid = p_death_covid/first(p_death_covid) )
  

