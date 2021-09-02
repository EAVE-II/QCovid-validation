##########################################################
# Name of file: Risk_prediction_models_hosp
# 
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Emily Moore Emily.Moore@phs.scot
# Latest update date (if not using version control) - 10/12/2020
# Latest update description (if not using version control) : 
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Reads in Qcovid cohort & end points. Data was prepped in the scripts 01_QCovid_descriptives.
#                         Fits a model using the variables from the QCovid BMJ paper (missing Chemo /radio data, at present)
#                         
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(dplyr)
#library(tidyft) - only call tidyft if/when needed as it masks lots of other functions
library(plyr)
library(tidyverse)
library(survival)
library(ggplot2)
library(tidyselect)
library(survival)
library(tidyr)


##02 Modelling ----- 
df <- readRDS("/conf/EAVE/GPanalysis/QCovid_Validation/data/Qcovid_prepped.rds")
source("/conf/EAVE/GPanalysis/progs/EM/Risk_prediction/Functions.R")
update_date <- "2020-11-10"

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
#df$CKD_level <- factor(df$CKD_level, ordered = TRUE, 
#                       levels = c("No CKD","CKD 3",   "CKD 4",   "CKD 5" ) )

#####

#Load new file for Q learn cat and Q_home_cat and Q_BMI.
#remove old vars 
df$Q_BMI <- NULL
df$Q_HOME_CAT <- NULL
df$Q_LEARN_CAT <- NULL



qcovid_addvars <- readRDS("/conf/EAVE/GPanalysis/data/QCOVID_BMIupdate.rds")

df <- left_join(df, qcovid_addvars, by = "EAVE_LINKNO")
# Modelling part 1:####
# model development on 1st wave (to 30th June)

#Censoring for hospitalisation: ####
end_date <- as.Date("2020-06-30")
#df$DATE_OF_DEATH <- NULL
a.begin <- as.Date("2020-03-01")
a.end.death <- end_date 
a.end.hosp <- end_date
a.end.test <- end_date 

##censor end dates/times to end_date of 1st wave####

#dftest <- df[1:5000]
df <- df %>% mutate(test_censored_date = if_else(SpecimenDate > a.end.test, a.end.test, as.Date(SpecimenDate)) ) %>%
  mutate(Time.To.Test = as.numeric(test_censored_date - a.begin))   %>%
  mutate(Death_censored_date = if_else(NRS.Date.Death > a.end.test, a.end.test, as.Date(NRS.Date.Death)) ) %>%
  mutate(Time.To.Death = as.numeric(Death_censored_date - a.begin)) %>%
  mutate(hosp_censored_date = if_else(date_hosp_covid > a.end.test, a.end.test, as.Date(date_hosp_covid)) ) %>%
  mutate(Time.To.Hosp = as.numeric(hosp_censored_date - a.begin))

df <- df %>% 
  mutate(Time.Test.Death = Time.To.Death - Time.To.Test,
         Time.Test.Hosp = Time.To.Hosp - Time.To.Test  )
df <- df %>% 
  mutate(Time.Test.Death = if_else(as.numeric(Time.Test.Death)<0,0,as.numeric(Time.Test.Death)),
         Time.Test.Hosp = if_else(as.numeric(Time.Test.Hosp)<0,0,as.numeric(Time.Test.Hosp)))

##change later hosps and deaths to 0.
table(df$hosp_covid, df$date_hosp_covid > end_date, useNA="always")

df <- df %>%  mutate(hosp_covid_censored = ifelse(date_hosp_covid > end_date, 0, hosp_covid), 
                     death_covid_censored = ifelse(NRS.Date.Death > end_date, 0, death_covid) )
               
                     

df <- df %>%  mutate(hosp_covid= if_else(Time.Test.Hosp > 28, 0, hosp_covid),
                         Time.Test.Hosp= if_else(Time.Test.Hosp > 28, 28, Time.Test.Hosp),
                         death_covid= if_else(Time.Test.Death > 40, 0, death_covid),
                         Time.Test.Death= if_else(Time.Test.Death > 40, 40, Time.Test.Death) )

z.var.list <- c("hosp_covid_censored", "death_covid_censored")
df <- df %>% mutate_at(z.var.list, ~replace(., is.na(.), 0))

#03a Hospitalisation - period to 30th June ----

#Variabels in BMJ article ####
#Hospitalisation, women

# Townsend = replace with SIMD rank
# Ethnicity (need to aggregate groups)
# Carehome/homeless  - only have 0/1 score yet
# Learning disability  - only have 0/1 score yet
# CKD - do not have transplant / dialisis info
# Chemo - dont have
# Blood cancer - 0/1
# Bone marrow/stem cel trnsplant - dont have
# Resp tract cancer - dont have
# sold organ transplant - dont have
# Imm suppressant - yes
# Leukotrine  - Y??
# oral steroids ??
# sickle cell - y
#Diabetes (yes)
# COPD - y
# Asthma - y
# rae lung cond. - Y
# pulm HT   Yes
# CHD
# Stroke
# AF
# Congestive HF
# thrombo embolism Q_DIAG_VTE
# PVD
# Congenital HD
# Dementia,
# Parkinsons
# Epilepsey
# Moterneurone
# Cerebral palsy
# Severe ment ill
# Osteo fracture
# Rheumatios arthritis Q_DIAG_RA_SLE
# Cirrhosis

z.var.list <- names(df)
z.var.list <- z.var.list[grepl("^Q_DIAG", z.var.list)]
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD3")] # remvvove individual CKD scores
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD4")]
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD5")]

z.var.list <- c(z.var.list, "CKD_level","new_ethnic_grp"  ,  "Q_RX_IMMUNO",  # add ethnicity, age, ckd level, prescribing variables
                "Q_RX_LABA", "Q_RX_PRED", 
                 "Q_HOME_CAT" , "Q_LEARN_CAT") 

table(is.na(df$CKD_level))
##NA --> 0 for all binary categorical variables
df <- df%>% mutate_at(z.var.list, ~replace(., is.na(.), 0)) 

df$Q_LEARN_CAT <- as.character(df$Q_LEARN_CAT)
df$Q_HOME_CAT <- as.character(df$Q_HOME_CAT)
#end variable selection.


#summary(df[,z.var.list])
#Response var
z.rv <- "hosp_covid_censored" 
z.rv.time <- "Time.Test.Hosp" 

#######FEMALES - HOSPITALISATION ####################################
df_F <- df %>% filter(Sex=="F")
meanbmi <- mean(df_F$BMI, na.rm=T)
plot(df_F$BMI, df_F$Age)

df_F <- df_F %>% group_by(agegrp) %>% mutate(meanBMIage = mean(BMI, na.rm=T)) %>% ungroup()

hosp_cases <- df_F %>% filter(hosp_covid_censored==1 )
ncontrols <- dim(hosp_cases)[1]*50
controls <- df_F %>% filter(hosp_covid_censored==0) %>% dplyr::slice_sample( n = ncontrols)

df.fit_F <- rbind(hosp_cases, controls)

table(df.fit_F$SIMD5, useNA = "always")


##NA BMI --> mean BMI for females.
df.fit_F$BMI <- ifelse(is.na(df.fit_F$BMI), df.fit_F$meanBMIage, df.fit_F$BMI)
#df.fit_F$SIMDrank <- ifelse(is.na(df.fit_F$SIMDrank), )


fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(Time.To.Test, df=4) + pspline(BMI, df=4) +  SIMD5  "
                               ))

df.fit_F$SIMD5 <- as.character(df.fit_F$SIMD5)
df.fit_F$CKD_level <- as.character(df.fit_F$CKD_level)
df.fit_F$CKD_level <- ifelse(df.fit_F$CKD_level =="No CKD", "0 No CKD",df.fit_F$CKD_level)
df.fit_F$new_ethnic_grp <- ifelse(df.fit_F$new_ethnic_grp =="White", "1 White",df.fit_F$new_ethnic_grp)
#z.var.list <- c(z.var.list, "SIMD5")
fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(Time.To.Test, df=4) + pspline(BMI, df=4) + SIMD5 +",
                               paste(z.var.list, collapse= "+")))
z.fit.F.H <- coxph(fmla.final , data= df.fit_F)
summary(z.fit.F.H)

termplot(z.fit.F.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=3, se=TRUE, col.term=1, col.se=1)



####### MALES - HOSPITALISATION ####################################
df_M <- df %>% filter(Sex=="M")
df_M <- df_M %>% group_by(agegrp) %>% mutate(meanBMIage = mean(BMI, na.rm=T)) %>% ungroup()

hosp_cases <- df_M %>% filter(hosp_covid_censored==1 )
ncontrols <- dim(hosp_cases)[1]*50
controls <- df_M %>% filter(hosp_covid_censored==0) %>% dplyr::slice_sample( n = ncontrols)

df.fit_M <- rbind(hosp_cases, controls)
df.fit_M$BMI <- ifelse(is.na(df.fit_M$BMI), df.fit_M$meanBMIage, df.fit_M$BMI)

df.fit_M$SIMD5 <- as.character(df.fit_M$SIMD5)
df.fit_M$CKD_level <- as.character(df.fit_M$CKD_level)
df.fit_M$CKD_level <- ifelse(df.fit_M$CKD_level =="No CKD", "0 No CKD",df.fit_M$CKD_level)
df.fit_M$new_ethnic_grp <- ifelse(df.fit_M$new_ethnic_grp =="White", "1 White",df.fit_M$new_ethnic_grp)

fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(Time.To.Test, df=4) + pspline(BMI, df=4) +  SIMD5 + ",
                               paste(z.var.list, collapse= "+")))
z.fit.M.H <- coxph(fmla.final , data= df.fit_M)
summary(z.fit.M.H)

termplot(z.fit.M.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=3, se=TRUE, col.term=1, col.se=1)


ptemp <- termplot(z.fitM, se=TRUE, plot=FALSE)
timeterm <- ptemp$Time.To.Test  # this will be a data frame
center <- with(timeterm, y[x==31])
ytemp <- timeterm$y + outer(timeterm$se, c(0, -1.96, 1.96),'*')
matplot(timeterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        xlab="Time of Positive Test - days from March 01", ylab="Relative hospitalisation rate", 
        main = "Specimen Date to 23rd June df=4")
abline(h=1, lty=2)

ptemp <- termplot(z.fitM, se=TRUE, plot=FALSE)
timeterm <- ptemp$BMI  # this will be a data frame
center <- with(timeterm, y[x==25])
ytemp <- timeterm$y + outer(timeterm$se, c(0, -1.96, 1.96),'*')
matplot(timeterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        xlab="BMI", ylab="Relative hospitalisation rate", 
        main = "Specimen Date to 23rd June df=4")
abline(h=1, lty=2)


###DEATHS##################
###DEATHS FEMALES##############################
table(df.fit_F$death_covid_censored, useNA="always")
#Response var
z.rv <- "death_covid_censored" 
z.rv.time <- "Time.Test.Death" 

fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(Time.To.Test, df=4) + pspline(BMI, df=4) +  SIMD5 + ",
                               paste(z.var.list, collapse= "+")))
z.fit.F.D <- coxph(fmla.final , data= df.fit_F)
summary(z.fit.F.D)


#DEATHS MALES ###############################

#Response var
z.rv <- "death_covid_censored" 
z.rv.time <- "Time.Test.Death" 

fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(Time.To.Test, df=4) + pspline(BMI, df=4) +  SIMD5 + ",
                               paste(z.var.list, collapse= "+")))
z.fit.M.D <- coxph(fmla.final , data= df.fit_M)
summary(z.fit.M.D)

##############END MODELS###########################################


###Results tables##########

##hosp females
STAT <- summary(z.fit.F.H)$coef
CI <- summary(z.fit.F.H)$conf.int
#"term"      "estimate"  "std.error" "statistic" "p.value"   "conf.low"  "conf.high"
term <- rownames(STAT)[7:53]
estimate <- as.numeric(CI[37:83,1])
conf.low<- as.numeric(CI[37:83,3])
conf.high<- as.numeric(CI[37:83,4])
std.error <- as.numeric(STAT[7:53,2])
statistic <- as.numeric(STAT[7:53,4])
p.value <-as.numeric(STAT[7:53,6])
mod.df <- as.data.frame(cbind(term, estimate,std.error ,statistic, p.value ,conf.low, conf.high))

length(estimate)
vars <- c(  "estimate",  "std.error" ,"statistic", "p.value" ,  "conf.low" , "conf.high")
mod.df <-mod.df %>% mutate_at(vars, as.character) %>% mutate_at(vars, as.numeric)
mod.df$term <- as.character(mod.df$term)

levels(df.fit_F$SIMD5)
knitr::kable(mod.df)
