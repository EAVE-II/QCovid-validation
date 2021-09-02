z <- df %>% dplyr::select(Q_ETHNICITY_0:Q_ETHNICITY_9)
z$tot <- apply(z,1,sum)

z1 <- filter(z, tot==2)


df.risk[1, 1:11]


fun_calc_score <- function(df, type){
  #type is "hosp" or "death"
  #df is a data.frame with 1 row and 42 columns, 1 id, 2, sex, 3:42 input cols for QCovid
  sex <- as.character(df$Sex)
  x <- x <- as.numeric(df[1,3:42])
  if (type=="hosp") z.res <- if (sex =="F") hospital_female(x) else hospital_male(x)
  if (type=="death") z.res <- if (sex =="F") death_female(x) else death_male(x)
  z.out <- data.frame(sex=sex, type=type, score=z.res)
}


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

z <- df %>% dplyr::select(Age, Sex, BMI) %>% group_by(Age,Sex) %>% 
  dplyr::summarise(meanBMI=round(mean(BMI,na.rm=T)))

df <- df %>% left_join(z, by=c("Age","Sex"))
df <- df %>% mutate(BMI = ifelse(is.na(BMI), meanBMI, BMI))
df$meanBMI <- NULL

df <- df %>% mutate(town = ifelse(is.na(town), mean(town, na.rm=T), town))


#get df in the correct order for the algorithm

df2 <- df %>% 
#  slice_sample(df, n=10) %>% 
  mutate(surv=91, chemocat=1) %>% 
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
                                 p_solidtransplant, death_covid, covid_cod, Time.To.Death, hosp_covid, Time.To.Hosp) %>% 
  mutate(Q_HOME_CAT = Q_HOME_CAT + 1,
         Q_LEARN_CAT= Q_LEARN_CAT + 1)

df.risk.f <- filter(df.risk, Sex=="F")
df.risk.m <- filter(df.risk, Sex=="M")

#z <- ddply(df.risk, .(EAVE_LINKNO), fun_calc_score, type="hosp")

#x <- slice_sample(df.risk.f, n=100)

z <- death_female(df.risk.f[3:42])
z <- as.data.frame(z)
names(z) <- c("Qrisk_death","a.death")
df.risk.f <- bind_cols(df.risk.f,z)
z <- hospital_female(df.risk.f[3:42])
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

#z <- subset(df.risk.f, Qrisk_death >= 80)

#df2 <- df2 %>%  left_join(dplyr::select(z, -sex, -type))

#df2.sub <- slice_sample(df2, n=100000)

Epi::ROC(test=df.risk.f$Qrisk_death, stat= df.risk.f$covid_cod, plot="ROC")

#z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ score, data=df2)
#concordance(z.fit)

z <- concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=df.risk.f,reverse=TRUE)
z.res.hf <- c(z$concordance, sqrt(z$var))
#concordance(Surv(Time.To.Death, death_covid) ~ Qrisk_death, data=df.risk.f, reverse=TRUE)
z <- concordance(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.f, reverse=TRUE)
z.res.df <- c(z$concordance, sqrt(z$var))
z <- concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=df.risk.m, reverse=TRUE)
z.res.hm <- c(z$concordance, sqrt(z$var))
#concordance(Surv(Time.To.Death, death_covid) ~ Qrisk_death, data=df.risk.m, reverse=TRUE)
z <- concordance(Surv(Time.To.Death, covid_cod) ~ Qrisk_death, data=df.risk.m, reverse=TRUE)
z.res.dm <- c(z$concordance, sqrt(z$var))
z.res <- rbind(z.res.hf, z.res.df, z.res.hm, z.res.dm)
z.res <- data.frame(Group=c("Hosp Female","Death_Female","Hosp_Male","Death_Male"),
                    Concordance=as.numeric(as.character(z.res[,1])), 
                    SE_C=as.numeric(as.character(z.res[,2])))
z.res <- z.res %>% mutate(Con_LCL=Concordance - 1.96*SE_C,Con_UCL=Concordance + 1.96*SE_C ) 

saveRDS(z.res, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_Concordance_P2.rds"))



z <- slice_sample(df.risk.f, n=10000)
Hmisc::rcorrcens(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=z)
concordance(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=z,reverse=TRUE)

z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ Qrisk_hosp, data=df.risk.f, ties="breslow")
royston(z.fit)

Hmisc::rcorr.cens(z$Qrisk_hosp, Surv(z$Time.To.Hosp, z$hosp_covid))
Hmisc::rcorrcens(Surv(Time.To.Death, death_covid) ~ Qrisk_death, data=df.risk.f)
Hmisc::somers2(df.risk.f$Qrisk_hosp, df.risk.f$hosp_covid)
Hmisc::somers2(df.risk.f$Qrisk_death, df.risk.f$death_covid)


z <- slice_sample(df.risk.f, n=100000)
z.markers = list(Qrisk_hosp=z$Qrisk_hosp)
riskRegression::Score(z.markers,formula=Hist(Time.To.Hosp, hosp_covid)~1,data=z,metrics=c("AUC","Brier"),
                      summary="ipa", times=max(z$Time.To.Hosp)-31)
z.markers = list(Qrisk_death=z$Qrisk_death)
riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,metrics=c("AUC","Brier"),
                      summary="ipa", times=max(z$Time.To.Death)-31)
riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,metrics=c("AUC"),
                      conf.int=TRUE, times=c(60,90))

riskRegression::Score(z.markers,formula=Hist(Time.To.Death, death_covid)~1,data=z,summary="ibs",
                      conf.int=TRUE, times=sort(unique(z$Time.To.Death)) )

library(pec)
z.markers = list(Qrisk_hosp=matrix(z$Qrisk_hosp, ncol=1))
z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ Age + town + BMI, data=z, x=TRUE, y=TRUE)
z1 <- pec::pec(z.fit, formula=Surv(Time.To.Hosp, hosp_covid)~1,data=z)
plot(z1)