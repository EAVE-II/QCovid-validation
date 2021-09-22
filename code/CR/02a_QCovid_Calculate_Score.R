##########################################################
# Name of file: 02a_QCovid_Calculate_Score.R
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
#                         02_QCovid_prediction_vectorised.R      
#                         run 01b_Modify_Endpoints.R first to select out the validation period
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#run 01_QCovid_Descriptions.R to set up data frame (df)
#Libraries
library(plyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

#read in the risk cacluation functions
source(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/progs/CR/02_QCovid_prediction_vectored.R"))

df <- df %>% mutate(town = ifelse(is.na(town), mean(town, na.rm=T), town))

#z <- df %>% dplyr::select(Age, Sex, BMI) %>% group_by(Age,Sex) %>% 
#  dplyr::summarise(meanBMI=round(mean(BMI,na.rm=T)))

#df <- df %>% left_join(z, by=c("Age","Sex"))
#df <- df %>% mutate(BMI = ifelse(is.na(BMI), meanBMI, BMI))
#df$meanBMI <- NULL

source("02a_QCovid_Impute_BMI.R")


#get df in the correct order for the algorithm

df2 <- df %>% 
#  slice_sample(df, n=10) %>% 
  mutate(surv=if_else(z.period=="P1",91,60),  #91/60
         chemocat=1) %>% 
  mutate(ethrisk= case_when(Q_ETHNICITY_1 == 1 | Q_ETHNICITY_2 == 1 | Q_ETHNICITY_3 == 1 | Q_ETHNICITY_4 == 1 ~ 2,  # White
                            Q_ETHNICITY_5 == 1 | Q_ETHNICITY_6 == 1 | Q_ETHNICITY_7 == 1 | Q_ETHNICITY_8 == 1 ~ 2,  # White
                            Q_ETHNICITY_8 == 1 ~ 3,  #Indian
                            Q_ETHNICITY_9 == 1 ~ 4,  #Pakistani
                            Q_ETHNICITY_10 == 1 ~ 5, #Bangladeshi
                            Q_ETHNICITY_11 == 1 ~ 6, #Other Asian
                            Q_ETHNICITY_12 == 1 ~ 7, #Carribean
                            Q_ETHNICITY_13 == 1 ~ 8, #Black African
                            Q_ETHNICITY_15 == 1 ~ 9, #Chinese
                            Q_ETHNICITY_15 == 1 ~ 10, #other
                            TRUE ~1) ) %>%  # not recorded - treated as White
  mutate(renalcat = case_when(Q_DIAG_CKD3 == 1 ~ 3,
                              Q_DIAG_CKD4 == 1 ~ 4,
                              Q_DIAG_CKD5 == 1 ~ 5, # no 5/6 - 7 levels in code but 6 in web/paper
                              TRUE ~ 1)) %>% 
  mutate(p_marrow6 = 0, p_radio6 = 0, p_solidtransplant =0) 

df2 <- df2 %>% 
  mutate(BMI = if_else(is.na(BMI), bmi_impute, BMI))

# there are a few with 2 ethicity values = pretty well all at type 3 along with type 1

#df2 has too many people in it
#keep everyone who has an event 
df2 <- df2 %>% mutate(keep = !is.na(SpecimenDate) | hosp_covid==1 | death_covid == 1 | icu_death ==1)
df2 <- df2 %>%  mutate(prob = ifelse(keep==1 & !is.na(keep),1,eave_weight * 0.98)) %>% 
  mutate(sel = rbinom(nrow(df2), size=1, prob=prob))
df2 <- df2 %>% filter(sel==1) %>% 
  dplyr::select(-keep, -prob, -sel)
                            

df.risk <- df2 %>% dplyr::select(EAVE_LINKNO, Sex, Age, BMI, town, surv, chemocat,ethrisk, Q_HOME_CAT, Q_LEARN_CAT, renalcat,
                                 Q_RX_IMMUNO, 
                                 Q_RX_LABA, 
                                 Q_RX_PRED,
                                 Q_DIAG_AF,
                                 Q_DIAG_CCF,
                                 Q_DIAG_ASTHMA,
                                 Q_DIAG_BLOOD_CANCER,
                                 Q_DIAG_CEREBRALPALSY,
                                 Q_DIAG_CHD,
                                 Q_DIAG_CIRRHOSIS,
                                 Q_DIAG_CONGEN_HD,
                                 Q_DIAG_COPD,
                                 Q_DIAG_DEMENTIA,
                                 Q_DIAG_EPILEPSY,
                                 Q_DIAG_FRACTURE,
                                 Q_DIAG_NEURO,
                                 Q_DIAG_PARKINSONS,
                                 Q_DIAG_PULM_HYPER,
                                 Q_DIAG_PULM_RARE,
                                 Q_DIAG_PVD,
                                 Q_DIAG_RA_SLE,
                                 Q_DIAG_RESP_CANCER,
                                 Q_DIAG_SEV_MENT_ILL,
                                 Q_DIAG_SICKLE_CELL,
                                 Q_DIAG_STROKE,
                                 Q_DIAG_DIABETES_1,
                                 Q_DIAG_DIABETES_2,
                                 Q_DIAG_VTE,
                                 p_marrow6,
                                 p_radio6,
                                 p_solidtransplant, death_covid, covid_cod, Time.To.Death, hosp_covid, Time.To.Hosp, hosp_remove) %>% 
  mutate(Q_HOME_CAT = Q_HOME_CAT + 1,
         Q_LEARN_CAT= Q_LEARN_CAT + 1)

df.risk.f <- filter(df.risk, Sex=="F")
df.risk.m <- filter(df.risk, Sex=="M")

#z <- ddply(df.risk, .(EAVE_LINKNO), fun_calc_score, type="hosp")

#x <- slice_sample(df.risk.f, n=100)

z <- death_female(df.risk.f[,3:42])
z <- as.data.frame(z)
names(z) <- c("Qrisk_death","a.death")
df.risk.f <- bind_cols(df.risk.f,z)
z <- hospital_female(df.risk.f[,3:42])
z <- as.data.frame(z)
names(z) <- c("Qrisk_hosp","a.hosp")
df.risk.f <- bind_cols(df.risk.f,z)

z <- death_male(df.risk.m[3:42])
z <- as.data.frame(z)
names(z) <- c("Qrisk_death","a.death")
df.risk.m <- bind_cols(df.risk.m,z)
z <- hospital_male(df.risk.m[3:42])
z <- as.data.frame(z)
names(z) <- c("Qrisk_hosp","a.hosp")
df.risk.m <- bind_cols(df.risk.m,z)



calc_OE <- function(df){
  death <- df %>% summarise_at( c('Qrisk_death', 'death_covid'), sum) %>%
    mutate_at( 'Qrisk_death' , ~ ./100) %>%
    mutate( death = round(death_covid/Qrisk_death, 2)) %>%
    select(death)
  
  
  hosp <- df %>% filter(hosp_remove == 0) %>%
    summarise_at(c('Qrisk_hosp', 'hosp_covid'), sum) %>%
    mutate_at( 'Qrisk_hosp', ~ ./100) %>%
    mutate(hosp = round(hosp_covid/Qrisk_hosp  , 2)) %>%
    select(hosp)
  
  output <- cbind(hosp,death)
  
  return(output)
}

male_OE <- calc_OE(df.risk.m)
names(male_OE) <- paste0(names(male_OE), '_male')

female_OE <- calc_OE(df.risk.f)
names(female_OE) <- paste0(names(female_OE), '_female')

OE <- cbind(female_OE, male_OE)

write.csv(OE, paste0('../../output/OE_', z.period, '.csv'))

#recalibrate once for females and again for males
z.df <- df.risk.f
#z.df <- df.risk.m
z.df <- z.df %>% 
  mutate(Qrisk_death_orig = Qrisk_death,
         Qrisk_hosp_orig = Qrisk_hosp)
#deaths
z_totals <- apply(z.df[,c("death_covid","Qrisk_death")], 2, sum)/c(1,100)
z_totals
z.df <- z.df %>% 
  mutate(Qrisk_death = Qrisk_death_orig*z_totals["death_covid"]/z_totals["Qrisk_death"])
#hospitalisations - note this will create  NA for those previously in hospital 
z_totals <- apply(z.df[z.df$hosp_remove==0,c("hosp_covid","Qrisk_hosp")], 2, sum)/c(1,100)
z_totals
z.df <- z.df %>% mutate(Qrisk_hosp_orig = Qrisk_hosp) %>% 
  mutate(Qrisk_hosp = if_else(hosp_remove==0, 
                              Qrisk_hosp_orig*z_totals["hosp_covid"]/z_totals["Qrisk_hosp"],
                              NA_real_))
df.risk.f <- z.df
#df.risk.m <- z.df


saveRDS(df.risk.m, paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_df_risk_m_",z.period,".rds"))
saveRDS(df.risk.f, paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_df_risk_f_",z.period,".rds"))
