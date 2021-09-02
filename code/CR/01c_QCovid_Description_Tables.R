##########################################################
# Name of file: 01c_QCovid_Description_Tables.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@nhs.scot
# Original date: 07 December 2020
# Latest update author (if not using version control) - Chris Robertson chris.robertson@nhs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: run 01b_modify_Endpoints.R chainging to do period 1 then 2
#                         runs through tabulations and graphs
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
###Comorbidities table
#df <- readRDS(paste0(Location,"EAVE/GPanalysis/QCovid_Validation/output/temp/CR_Qcovid_prepped.rds"))
print(z.period)
vars <- names(df)[grepl("Q_DIAG",names(df) )]
vars <- c(vars, "CKD_level" )
vars <- vars[!grepl("Q_DIAG_CKD", vars)]

z.df <- df %>% 
  dplyr::select_at(all_of(c(vars,"eave_weight", "hosp_covid","death_covid", "covid_cod", "Time.To.Hosp","Time.To.Death" ))) %>%
  mutate_at(all_of(vars), ~ as.factor(.))
z.long <- pivot_longer(z.df, cols= all_of(vars), names_to="Var", values_to = "Values")

#x <- filter(z.long, Var=="CKD_level")
fun.desc_1 <- function(x){
  z.df <- dplyr::select(x, Var, Values, eave_weight, 
                        hosp_covid, death_covid, covid_cod, Time.To.Hosp, Time.To.Death) %>% 
    group_by(Var, Values) %>% summarise(N.Crude = n(), N=round(sum(eave_weight)), 
                                hosp_covid=sum(hosp_covid) , death_covid=sum(death_covid),
                                covid_cod=sum(covid_cod),
                                time_hosp = round(sum(eave_weight*Time.To.Hosp)),
                                time_death = round(sum(eave_weight*Time.To.Death))) %>% 
    mutate(p_hosp_covid = hosp_covid/N, p_death_covid = death_covid/N, p_covid_cod=covid_cod/N,
           rate_hosp_100 = hosp_covid/time_hosp*100,
           rate_death_100 = death_covid/time_death*100,
           rate_covid_cod_100 = covid_cod/time_death*100) %>% 
    mutate(r_hosp_covid = p_hosp_covid/first(p_hosp_covid), 
           r_death_covid = p_death_covid/first(p_death_covid),
           r_covid_cod = p_covid_cod/first(p_covid_cod),
           rate_ratio_hosp_covid = rate_hosp_100/first(rate_hosp_100), 
           rate_ratio_death_covid = rate_death_100/first(rate_death_100),
           rate_ratio_covid_cod = rate_covid_cod_100/first(rate_covid_cod_100)) %>% 
    mutate(Percent = N/sum(N)*100, Percent_hosp=hosp_covid/sum(hosp_covid)*100,
           Percent_dead=death_covid/sum(death_covid)*100,
           Percent_covid_cod=covid_cod/sum(covid_cod)*100) %>% 
    as.data.frame()
}

z.out <- plyr::ddply(z.long, ~Var, fun.desc_1)

#change p1/p2 as needed
saveRDS(z.out, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab1_clinical_",z.period,".rds"))
#need to select out risk for the top 5
#saveRDS(z.out, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab1_clinical_top5.rds"))

#vars <- c("Sex","agegrp","new_ethnic_grp","town_gp","BMI_gp")
vars <- c("Sex","agegrp","town_gp","BMI_gp")
z.df <- df %>% 
  dplyr::select_at(all_of(c(vars,"eave_weight", "hosp_covid","death_covid", "covid_cod", "Time.To.Hosp","Time.To.Death" ))) 
z.long <- pivot_longer(z.df, cols= all_of(vars), names_to="Var", values_to = "Values")

z.out <- plyr::ddply(z.long, ~Var, fun.desc_1)
saveRDS(z.out, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab2_demog_",z.period,".rds"))
#saveRDS(z.out, paste0(Location, "EAVE/GPanalysis/QCovid_Validation/output/CR_Qcovid_descr_tab2_demog_top5.rds"))

#z.sub <- filter(z.long, Var=="EAVE_ASTHMA")
fun.fit <- function(df, time_var, status_var){ 
  z.fit <- coxph(Surv(get(time_var), get(status_var)) ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ur6_2016_name + Values, data=df)
  z.t <- as.data.frame(summary(z.fit)$conf.int)
  z.sel <- grepl("Values", rownames(z.t)) 
  z.t <- z.t[z.sel,c(1,3,4)]
  colnames(z.t) <- c("HR","LCL","UCL")
  z.t <- cbind(Var=rep(unique(df$Var), nrow(z.t)) , Name=rownames(z.t), Response=rep(status_var,nrow(z.t)), z.t)
  z.t
}
#fun.fit(z.sub, time_var="Time.To.Death", status_var="death_covid")

z.out.dth <- plyr::ddply(z.long, .(Var), fun.fit, time_var="Time.To.Death", status_var="death_covid")


