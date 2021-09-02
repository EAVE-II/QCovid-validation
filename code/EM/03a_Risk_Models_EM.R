##########################################################
# Name of file: 03a_Risk_models.R
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
source(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/progs/CR/01b_Modify_Endpoints.R"))

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

table(df_M$death_covid, useNA = "always")


#summary(df[,z.var.list])

#######FEMALES  ####################################
#Response var
z.rv <- "hosp_covid" 
z.rv.time <- "Time.To.Hosp" 
df_F <- filter(df, Sex=="F")
cases <- df_F %>% filter(get(z.rv)==1 )
ncontrols <- dim(cases)[1]*100
controls <- df_F %>% filter(get(z.rv)==0) %>% dplyr::slice_sample( n = ncontrols)

df.fit_F <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))
fmla.1
z.fit.F.H <- coxph(fmla.1 , data= df.fit_F)
summary(z.fit.F.H)
ptemp <- termplot(model = z.fit.F.H,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_F_H.rds"))
        
termplot(z.fit.F.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.H, term=3, se=TRUE, col.term=1, col.se=1)

saveRDS(z.fit.F.H, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Model_Hosp_Female.rds"))

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
summary(z.fit.F.D)
ptemp <- termplot(model = z.fit.F.D,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_F_D.rds"))



saveRDS(z.fit.F.D, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Model_Death_Female.rds"))
termplot(z.fit.F.D, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=3, se=TRUE, col.term=1, col.se=1)

######### Males ##################
#Response var
z.rv <- "hosp_covid" 
z.rv.time <- "Time.To.Hosp" 
df_M <- filter(df, Sex=="M")
cases <- df_M %>% filter(get(z.rv)==1 )####WRONG
ncontrols <- dim(cases)[1]*100
controls <- df_M %>% filter(get(z.rv)==0) %>% dplyr::slice_sample( n = ncontrols)

df.fit_M <- rbind(cases, controls)

fmla.1 <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(Age) + pspline(town) + pspline(BMI) + ",
                           paste(z.var.list, collapse= "+")))
z.fit.M.H <- coxph(fmla.1 , data= df.fit_M)
summary(z.fit.M.H)
ptemp <- termplot(model = z.fit.M.H,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_M_H.rds"))


termplot(z.fit.M.H, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.M.H, term=3, se=TRUE, col.term=1, col.se=1)
saveRDS(z.fit.M.H, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Hosp_Male.rds"))

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
summary(z.fit.M.D)

ptemp <- termplot(model = z.fit.M.D,   se=TRUE, plot=FALSE)
##save data for termplots - they need the data to run.
saveRDS(ptemp, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/temp/ptemp_M_D.rds"))


saveRDS(z.fit.M.D, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/EM_Model_Death_Male.rds"))
termplot(z.fit.F.D, term=1, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=2, se=TRUE, col.term=1, col.se=1)
termplot(z.fit.F.D, term=3, se=TRUE, col.term=1, col.se=1)


##splines####
ptemp <- termplot(z.fit.M.H, se=TRUE, plot=FALSE)
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
