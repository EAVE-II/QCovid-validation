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

QCovid_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/data/QCOVIDdeduped.rds"))
QCovid_cohort <- filter(QCovid_cohort, !duplicated(EAVE_LINKNO)) 
QCovid_cohort$Sex <- ifelse(QCovid_cohort$Sex=="2", "F", QCovid_cohort$Sex)
QCovid_cohort <- QCovid_cohort %>%  filter(DeductionDate >= as.Date("2020-03-01") | is.na(DeductionDate))

z.var.list <- names(QCovid_cohort)
z.var.list <- z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)]
z.var.list <- z.var.list[!(z.var.list %in% c("Q_HOME_CAT","Q_LEARN_CAT")) ]

QCovid_cohort <- QCovid_cohort %>% mutate_at(z.var.list, ~replace(., is.na(.), 0)) #replace missing values with zero
QCovid_cohort <- QCovid_cohort %>% mutate(any_comorb = select(., Q_DIAG_AF:Q_DIAG_VTE) %>% rowSums(na.rm = TRUE))
QCovid_cohort <- QCovid_cohort %>% mutate(any_other = select(., Q_RX_IMMUNO:SG_IMMRX_COD) %>% rowSums(na.rm = TRUE))
QCovid_cohort <- QCovid_cohort %>% mutate(any_ethnicity = select(., Q_ETHNICITY_1:Q_ETHNICITY_9) %>% rowSums(na.rm = TRUE))
QCovid_cohort <- QCovid_cohort %>% mutate(any_cats = !is.na(BMI) + !is.na(Q_HOME_CAT) + !is.na(Q_LEARN_CAT) ) 

#QCovid_cohort <- slice_sample(QCovid_cohort, n=50000)

table(QCovid_cohort$any_comorb, useNA="always")
table(QCovid_cohort$any_other, useNA="always")
table(QCovid_cohort$any_ethnicity, useNA="always")
table(QCovid_cohort$SG_ASTRX_COD, useNA="always")
table(QCovid_cohort$SG_IMMRX_COD, useNA="always")
table(QCovid_cohort$any_comorb, QCovid_cohort$any_other, QCovid_cohort$any_ethnicity, useNA="always")
table(QCovid_cohort$any_comorb, QCovid_cohort$any_ethnicity, useNA="always")


##Q covid does not contain persons with no Q codes AND not Q ethnicity codes - need to add data for these from the demographics table.
##ENdpoints

##Q covid does not contain persons with no Q codes AND not Q ethnicity codes - need to add data for these from the demographics table.
##ENdpoints
EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times2020-11-10.rds")) # 23 June endpoints
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
##demographics
EAVE_demog <- readRDS(paste0(Location,"EAVE/GPanalysis/data/EAVE_demographics_SK.rds")) 
EAVE_demog <- EAVE_demog %>% select(EAVE_LINKNO, Sex, ageYear,hb2019name , DataZone ) %>%
  dplyr::rename(SEX = Sex) # rename so dont join & create dups if sex differes

table(QCovid_cohort$EAVE_LINKNO %in% EAVE_cohort$EAVE_LINKNO)  #need tio check up on these ones

##left join demographics to the endpoints
z <- EAVE_cohort %>% select(EAVE_LINKNO, SpecimenDate:Time.To.Hosp ) 
z <- left_join(z,EAVE_demog)

table(QCovid_cohort$EAVE_LINKNO %in% z$EAVE_LINKNO)  #need tio check up on these ones

#df <- QCovid_cohort %>% left_join(z, by="EAVE_LINKNO")

##join q covid - keep ALL  in EAVE - DROP the 100,000 in QCovid but not EAVE
df <- left_join(z,QCovid_cohort)
df <- filter(df, !duplicated(EAVE_LINKNO))
#rm(check)
##resolve Age, sex
table(df$SEX, df$Sex, useNA="always")

df$Sex <- ifelse(is.na(df$Sex), df$SEX, df$Sex)
df$SEX <-NULL

table(df$Age, df$ageYear, useNA="always")

str(df$Age)
str(df$ageYear)
df$Age <- as.numeric(df$Age)

df$Age <- ifelse(is.na(df$Age), df$ageYear, df$Age)
df$ageYear <-NULL


df$datazone2011 <- ifelse(is.na(df$datazone2011), df$DataZone, df$datazone2011)
df$DataZone <-NULL


EAVE_weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
df <- df %>%
  left_join(EAVE_weights, by="EAVE_LINKNO")


# remove those under 19 and over 100
df <- df %>% filter(Age >= 19 & Age <= 100)

##############################################

## Prep variables ----
##CKD - convert to the worst
#table(df$Q_DIAG_CKD3)
df <- df %>% mutate(CKD_level = case_when(Q_DIAG_CKD3 == 0 & Q_DIAG_CKD4 == 0 & Q_DIAG_CKD5 == 0   ~ "No CKD", 
                                          Q_DIAG_CKD3 == 1 ~ "CKD 3",
                                          Q_DIAG_CKD4 == 1 ~ "CKD 4",
                                          Q_DIAG_CKD5 == 1 ~ "CKD 5",
                                          TRUE ~ "No CKD"
))


##Comvert ethnicity variables to one####

#table(df$any_ethnicity==2, df$Q_ETHNICITY_1)
#table(df$any_ethnicity==2, df$Q_ETHNICITY_2)
#table(df$any_ethnicity==2, df$Q_ETHNICITY_3)#

#table(df$Q_ETHNICITY_1, df$Q_ETHNICITY_3) # 70 british +other white-->   white
#table(df$Q_ETHNICITY_2, df$Q_ETHNICITY_1) # 2 irish & british-->   white
#table(df$Q_ETHNICITY_3, df$Q_ETHNICITY_16) # 1 "other white" + "other" -->   white
#table(df$Q_ETHNICITY_5, df$Q_ETHNICITY_13) # african & af/wht mixed  --> mixed
#Comvert to the categories in Q covid paper 
#White = (1,2,3)
#Indian = 8
#Pakisatani = 9
#Bangladeshi = 10
#Other asian = 11
#Carribean = 12
#Black african = 13
#Chinese = 15
#Other & mixed = 7, 16, 14, 4,5,6
#not recorded = 0 , 17

##how many end up in "other" when it is actually stated
# 7 = other mixed, 16  = other.
#table(df$Q_ETHNICITY_14) # nearly 7k may separate at least to begin
#table(df$Q_ETHNICITY_4)
#table(df$Q_ETHNICITY_5)
#table(df$Q_ETHNICITY_6)

df <- df %>% mutate(ethnic_grp = case_when(Q_ETHNICITY_1==1 | Q_ETHNICITY_2==1 |Q_ETHNICITY_3==1  ~ "White", 
                                           Q_ETHNICITY_8==1 ~ "Indian",
                                           Q_ETHNICITY_9==1 ~ "Pakistani",
                                           Q_ETHNICITY_10==1 ~ "Bangladeshi",
                                           Q_ETHNICITY_11==1 ~ "Other Asian", 
                                           Q_ETHNICITY_12 == 1 ~ "Carribean", 
                                           Q_ETHNICITY_13 == 1 ~ "Black African", 
                                           Q_ETHNICITY_14 == 1 ~ "Other Black", 
                                           Q_ETHNICITY_15 == 1 ~ "Chinese",
                                           Q_ETHNICITY_4 == 1 ~ "Mixed - White Black carrib",
                                           Q_ETHNICITY_5 == 1 ~ "Mixed - White Black African",
                                           Q_ETHNICITY_6 == 1 ~ "Mixed - White Asian",
                                           Q_ETHNICITY_16 == 1 | Q_ETHNICITY_7 == 1  ~ "Other",
                                           Q_ETHNICITY_0 == 1  ~ "Not recorded",
                                           TRUE ~ "Not recorded"
))
#table(df$ethnic_grp, useNA="always")



#Age groups
df <- df %>% mutate(agegrp = cut(Age, breaks = c(-1,29,39,49,59,69, 79, 89,121), 
                                 labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-80", "90+")))

##SIMD - I am not able to run - CR

# put added EAVE records whihc are missing for Qcovid groups to zero
z.var.list <- names(df)
z.var.list <- z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)]

df <- df %>% mutate_at(z.var.list, ~replace(., is.na(.), 0)) #replace missing values with zero

#saveRDS(df, paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_Qcovid_prepped.rds"))

#update_date <- "2020-11-10"

#final data prep ####

#Combine ethnicity to fit the BMJ categories
table(df$ethnic_grp)
df$new_ethnic_grp <- case_when(  df$ethnic_grp == "Mixed - White Asian" |
                                   df$ethnic_grp ==  "Mixed - White Black African" |
                                   df$ethnic_grp == "Mixed - White Black carrib" |
                                   df$ethnic_grp == "Other Black" ~ "Other", 
                                 TRUE ~ df$ethnic_grp
)
table(df$new_ethnic_grp, df$ethnic_grp)

##oredered factors for ethnicity, CKDl level
table(df$new_ethnic_grp)
#df$new_ethnic_grp <- factor(df$new_ethnic_grp, ordered = TRUE, 
#                            levels = c("White", "Indian", "Pakistani", 
#                                       "Bangladeshi",         "Other Asian", "Carribean", 
#                                       "Black African" , "Chinese"  , "Other" , "Not recorded"))

#table(df$CKD_level)
df$CKD_level <- factor(df$CKD_level, ordered=FALSE,
                       levels = c("No CKD","CKD 3",   "CKD 4",   "CKD 5" ) )

#####


z.levels_new_eth <- c("White", "Indian", "Pakistani", "Bangladeshi", "Chinese",
                "Other Asian", "Black African", "Carribean", "Other", "Not recorded")
df <- df %>% mutate(new_ethnic_grp = factor(new_ethnic_grp, levels=z.levels_new_eth),
                    Sex = factor(Sex, levels=c("F","M")),
                    BMI_gp = cut(BMI, breaks=c(0,19,24,29,34,39,51)))

z <- read.csv(paste0(Location, "EAVE/GPanalysis/QCovid_Validation/data/DZ_TLS.csv"))
z <- z %>% group_by(DataZone) %>% dplyr::summarise(town=median(X2011Code.TDS))

df <- df %>% left_join(z, by=c("datazone2011" = "DataZone") )

df <- df %>% mutate(town_gp = cut(town, breaks=quantile(town,c(0,0.2,0.4,0.6,0.8,1), na.rm=TRUE), 
                                  include.lowest=TRUE, labels=paste0("Q",1:5)))


saveRDS(df, paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_Qcovid_prepped.rds"))

rm(EAVE_cohort, EAVE_demog, EAVE_weights, z, qcovid_addvars, QCovid_cohort)
