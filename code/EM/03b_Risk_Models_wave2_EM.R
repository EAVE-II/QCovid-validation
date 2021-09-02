##########################################################
# Name of file: 03a_Risk_models_2ndwave.R
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
Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

df <- readRDS(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_Qcovid_prepped.rds"))

#source(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/progs/CR/01b_Modify_Endpoints.R"))
nhosp <- df %>% group_by(EAVE_LINKNO) %>%
  dplyr::summarise(n_records = n(), n_result = sum(result), n_hosp = sum(hosp_covid))
table(nhosp$n_records)
table(nhosp$n_result)
table(nhosp$n_hosp)

dfdup <- df %>% group_by(EAVE_LINKNO) %>% dplyr::mutate(count = n()) %>% filter(count > 1)
#select 1st event for each person
df <- df %>% arrange(EAVE_LINKNO, SpecimenDate, date_hosp_covid) %>%  group_by(EAVE_LINKNO) %>%
  slice(1) %>% ungroup()
#2nd wave - different trim of end points. 
#DEAD before start date == remove from cohort
#HOSP beofre start date == remove from HOSP model ONLY (they can still die!)
#Specimen date before start date - this is irrelevant to hosp and death models, so keep them in. 



update_date <- "2020-11-10"

#Censoring for hospitalisation/deaths: ####
#reset start date and times to begin from 1st july
# reset end dates to 
#end_date <- as.Date("2020-06-30")
#df$DATE_OF_DEATH <- NULL
a.begin <- as.Date("2020-07-01")
a.end.death <- max(df$DATE_OF_DEATH, na.rm = T)
a.end.hosp <- max(df$date_hosp_covid, na.rm = T)
a.end.test <- max(df$SpecimenDate, na.rm=T)
z.max.days.hosp <- as.numeric(a.end.hosp  - as.Date("2020-07-01"))
z.max.days.death <- as.numeric(a.end.death - as.Date("2020-07-01"))
z.max.days<- as.numeric(a.end.test - as.Date("2020-07-01"))
##censor end dates/times to end_date of 1st wave####
#remove if  dead before ist July
#FLAG if hosp before 1st july as these people need removing from the HOSPITAL model ONLY
df <- df %>%  filter(DATE_OF_DEATH >= a.begin | is.na(DATE_OF_DEATH)) %>%
  mutate(prev_hosp =  ifelse(date_hosp_covid < a.begin, 1,0), 
         mutate(hosp_covid = ifelse(date_hosp_covid < a.begin, 0 , hosp_covid )))# flag 1 to mark for removal from HOSP cohort only
df <- df %>% mutate_at("prev_hosp", ~replace(., is.na(.), 0))
table(df$prev_hosp)

#Resetting the Time.To variables and the censor dates/times
df <- df %>% mutate(#tested = ifelse(Time.To.Test > z.max.days, 0, tested),
                    #result = ifelse(Time.To.Test > z.max.days, 0, result),
                    Time.To.Test = SpecimenDate - a.begin, 
                    Time.To.Hosp = date_hosp_covid - a.begin,
                    Time.To.ICU.Death = date_icu_death - a.begin,
                    Time.To.Death = DATE_OF_DEATH - a.begin) %>%
  mutate(Time.To.Test = ifelse(is.na(SpecimenDate), a.end.test - a.begin, Time.To.Test), 
         Time.To.Hosp = ifelse(is.na(hosp_covid), a.end.hosp - a.begin, Time.To.Hosp),
         Time.To.ICU.Death = ifelse(is.na(icu_death), a.end.hosp - a.begin, Time.To.ICU.Death),
         Time.To.Death =ifelse(is.na(DATE_OF_DEATH), a.end.death - a.begin, Time.To.Death))

table(df2$Time.To.Death)
df <- df %>% mutate(Time.To.Hosp = ifelse(Time.To.Hosp < 0, 0, Time.To.Hosp) ) # set negatives to 0, although these will be removed from the hosp model anyhow.


#source("/conf/EAVE/GPanalysis/progs/EM/Risk_prediction/Functions.R")
update_date <- "2020-11-10"


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
z.var.list <- c("Q_HOME_CAT","Q_LEARN_CAT")
df <- df %>% mutate_at(z.var.list, ~replace(., is.na(.), 0))

df$Q_LEARN_CAT <- as.character(df$Q_LEARN_CAT)
df$Q_HOME_CAT <- as.character(df$Q_HOME_CAT)


z.var.list <- names(df)
z.var.list <- z.var.list[grepl("^Q_DIAG", z.var.list)]
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD3")] # remvvove individual CKD scores
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD4")]
z.var.list <- z.var.list[!(z.var.list=="Q_DIAG_CKD5")]

z.var.list <- c(z.var.list, "CKD_level","new_ethnic_grp"  ,  "Q_RX_IMMUNO",  # add ethnicity, age, ckd level, prescribing variables
                "Q_RX_LABA", "Q_RX_PRED", 
                "Q_HOME_CAT" , "Q_LEARN_CAT") 





#end variable selection.


#summary(df[,z.var.list])

#######FEMALES  ####################################
#Response var
z.rv <- "hosp_covid" 
z.rv.time <- "Time.To.Hosp" 
df_F <- filter(df, Sex=="F") 
cases <- df_F %>% #filter(prev_hosp==0) %>% ##REMOVE anyone already hosp.
  filter(get(z.rv)==1 )

ncontrols <- dim(cases)[1]*100
controls <- df_F %>%  #filter(prev_hosp==0) %>% ##REMOVE anyone already hosp.
                filter(get(z.rv)==0) %>% dplyr::slice_sample( n = ncontrols)

df.fit_F <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))
fmla.1
z.fit.F.H <- coxph(fmla.1 , data= df.fit_F)
#z.fit.F.H <- coxph(fmla.1 , data= df_F)
summary(z.fit.F.H)
ptemp <- termplot(model = z.fit.F.H,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_F_H2W.rds"))

termplot(z.fit.F.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=3, se=TRUE, col.term=1, col.se=1)

saveRDS(z.fit.F.H, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Hosp_Female2W.rds"))


##deaths ####
z.rv <- "death_covid" 
z.rv.time <- "Time.To.Death" 
df_F <- filter(df, Sex=="F")
cases <- df_F %>% filter(get(z.rv)==1 )
ncontrols <- dim(cases)[1]*100
controls <- df_F %>% filter(get(z.rv)==0) %>% dplyr::slice_sample( n = ncontrols)
df.fit_F <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))
z.fit.F.D <- coxph(fmla.1 , data= df.fit_F)
#z.fit.F.D <- coxph(fmla.1 , data= df_F)
summary(z.fit.F.D)
ptemp <- termplot(model = z.fit.F.D,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_F_D2W.rds"))



saveRDS(z.fit.F.D, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Death_Female2W.rds"))
termplot(z.fit.F.D, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=3, se=TRUE, col.term=1, col.se=1)

######### Males ##################
#Response var
z.rv <- "hosp_covid" 
z.rv.time <- "Time.To.Hosp" 
df_M <- filter(df, Sex=="M")

cases <- df_M %>% #filter(prev_hosp==0) %>% ##REMOVE anyone already hosp.
  filter(get(z.rv)==1 )

ncontrols <- dim(cases)[1]*100
controls <- df_M %>% #filter(prev_hosp==0) %>% ##REMOVE anyone already hosp.
  filter(get(z.rv)==0 ) %>% dplyr::slice_sample( n = ncontrols)

df.fit_M <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))
z.fit.M.H <- coxph(fmla.1 , data= df.fit_M)
#z.fit.M.H <- coxph(fmla.1 , data= df_M)
summary(z.fit.M.H)
ptemp <- termplot(model = z.fit.M.H,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_M_H2W.rds"))


termplot(z.fit.M.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=3, se=TRUE, col.term=1, col.se=1)
saveRDS(z.fit.M.H, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Hosp_Male2W.rds"))

z.rv <- "death_covid" 
z.rv.time <- "Time.To.Death" 
df_M <- filter(df, Sex=="M")
cases <- df_M %>% filter(get(z.rv)==1 )
ncontrols <- dim(cases)[1]*100
controls <- df_M %>% filter(get(z.rv)==0) %>% dplyr::slice_sample( n = ncontrols)
df.fit_M <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))

z.fit.M.D <- coxph(fmla.1 , data= df.fit_M)
#z.fit.M.D <- coxph(fmla.1 , data= df_M) # fit on all males
summary(z.fit.M.D)

ptemp <- termplot(model = z.fit.M.D,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_M_D2W.rds"))


saveRDS(z.fit.M.D, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Death_Male2W.rds"))
termplot(z.fit.F.D, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=3, se=TRUE, col.term=1, col.se=1)

