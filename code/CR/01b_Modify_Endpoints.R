##########################################################
# Name of file: 01b_modify_Endpoints.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Changes the endpoint date and also the event variables
#                         hosp_covid, death_covid Time.To.Hosp Time.To.Death
#                         keeps the same variable names though
# Approximate run time: Unknown
##########################################################
#
#Modified with Emily's version but don't do a full join to QCovid #############
#

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
#library(janitor)

#Load data ----
Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

df <- readRDS(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_Qcovid_prepped.rds"))
#source("/conf/EAVE/GPanalysis/progs/EM/Risk_prediction/Functions.R")
#update_date <- "2020-11-10"

#df <- df %>% dplyr::rename(DeductionDate = DeductionDate.x) %>% 
#  dplyr::select(-DeductionDate.y)

cohort_start_date <- as.Date("2020-03-01")
#Censoring for hospitalisation: ####
#end_date <- as.Date("2020-04-30")
end_date <- as.Date("2020-06-30")
z.period <-"P1" # second Period

#df$DATE_OF_DEATH <- NULL
a.begin <- as.Date("2020-03-01")  #  All who died or deducted before this date omitted
#a.begin <- as.Date("2020-05-01")  #  All who died or deducted before this date omitted
days_start_to_begin <-as.numeric(a.begin-cohort_start_date)
a.end.death <- end_date 
a.end.hosp <- end_date
a.end.test <- end_date
z.max.days <- as.numeric(end_date - a.begin)

##censor end dates/times to end_date of 1st wave####
df$covid_cod[is.na(df$covid_cod)] <- 0
#dftest <- df[1:5000,]
df <- df %>% mutate(tested = ifelse(Time.To.Test > z.max.days+days_start_to_begin, 0, tested),
                            result = ifelse(Time.To.Test > z.max.days+days_start_to_begin, 0, result),
                            hosp_covid = ifelse(Time.To.Hosp > z.max.days+days_start_to_begin, 0, hosp_covid),
                            icu_death = ifelse(Time.To.ICU.Death > z.max.days+days_start_to_begin, 0, icu_death),
                            death_covid = ifelse(Time.To.Death > z.max.days+days_start_to_begin, 0, death_covid), 
                            death28 = ifelse(Time.To.Death > z.max.days+days_start_to_begin, 0, death28), 
                            covid_cod = ifelse(Time.To.Death > z.max.days+days_start_to_begin, 0, covid_cod))  %>%
    mutate(Time.To.Test = Time.To.Test - days_start_to_begin,
           Time.To.Hosp = Time.To.Hosp - days_start_to_begin,
           Time.To.ICU.Death = Time.To.ICU.Death - days_start_to_begin,
           Time.To.Death = Time.To.Death - days_start_to_begin) %>% 
    mutate(Time.To.Test = ifelse(Time.To.Test > z.max.days, z.max.days, Time.To.Test),
           Time.To.Hosp = ifelse(Time.To.Hosp > z.max.days, z.max.days, Time.To.Hosp),
           Time.To.ICU.Death = ifelse(Time.To.ICU.Death > z.max.days, z.max.days, Time.To.ICU.Death),
           Time.To.Death = ifelse(Time.To.Death > z.max.days, z.max.days, Time.To.Death))
df <- df %>% mutate(Time.To.Hosp = ifelse(Time.To.Hosp < 0, 0, Time.To.Hosp) )

#remove any who died before a.begin
df <- df %>% filter(is.na(DATE_OF_DEATH) | DATE_OF_DEATH >= a.begin) %>% 
  filter(is.na(DeductionDate) | DeductionDate >= a.begin)

#set up a hospital flag to remove individuals with a previous hospitalisation from the 
#hospital validation
df <- df %>% mutate(hosp_remove = case_when(!is.na(date_hosp_covid) &
                              (hosp_covid==1) & (date_hosp_covid < a.begin)  ~ 1,
                              TRUE ~ 0))

#use covid_cod as the primary death - death with covid on certificate
#secondary using all deaths - death_covid
#hosp is hosp_covid and include people hospitalised for covid but without testiong positive

