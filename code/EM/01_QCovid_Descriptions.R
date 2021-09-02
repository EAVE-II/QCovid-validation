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

# 01 Setup ####
#Libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(survival)
library(janitor)

#Load data ----
Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

QCovid_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/data/QCOVIDdeduped.rds"))
QCovid_cohort <- filter(QCovid_cohort, !duplicated(EAVE_LINKNO)) %>% 
  dplyr::select(-DateOfBirth) 
QCovid_cohort$Sex <- ifelse(QCovid_cohort$Sex=="2", "F", QCovid_cohort$Sex)
QCovid_cohort <- QCovid_cohort %>%  filter(DeductionDate >= as.Date("2020-03-01") | is.na(DeductionDate))

z.var.list <- names(QCovid_cohort)
z.var.list <- z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)| grepl("BMI", z.var.list)]

QCovid_cohort <- QCovid_cohort %>% mutate_at(z.var.list, ~replace(., is.na(.), 0)) #replace missing values with zero
QCovid_cohort <- QCovid_cohort %>% mutate(any_comorb = select(., Q_DIAG_AF:Q_DIAG_VTE) %>% rowSums(na.rm = TRUE))
QCovid_cohort <- QCovid_cohort %>% mutate(any_other = select(., Q_HOME_CAT:SG_IMMRX_COD) %>% rowSums(na.rm = TRUE))
QCovid_cohort <- QCovid_cohort %>% mutate(any_ethnicity = select(., Q_ETHNICITY_1:Q_ETHNICITY_9) %>% rowSums(na.rm = TRUE))

table(QCovid_cohort$any_comorb, useNA="always")
table(QCovid_cohort$any_other, useNA="always")
table(QCovid_cohort$SG_ASTRX_COD, useNA="always")
table(QCovid_cohort$SG_IMMRX_COD, useNA="always")

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

##join q covid - keep ALL
df <- full_join(z,QCovid_cohort)
check <- filter(df, !duplicated(EAVE_LINKNO))
rm(check)
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

##remove NAs
z.var.list <- names(QCovid_cohort)
z.var.list <- z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)| grepl("BMI", z.var.list)]

df <- df %>% mutate_at(z.var.list, ~replace(., is.na(.), 0))
##rerun weights##
Scotland_Population <- readRDS("/conf/EAVE/GPanalysis/data/Population_2019_CA_age_sex.rds")
Scotland_Population <- Scotland_Population %>% group_by(age, sex_name) %>% 
  dplyr::summarize(pop=sum(pop)) %>% ungroup() %>% 
  rename(sex=sex_name)

###take the "df" numbers
Eave_Population <- df %>% mutate(Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Age, Sex) %>% 
  dplyr::summarize(pop_eave=n()) %>% ungroup() %>% 
  rename(age=Age, sex=Sex)

df.w <- left_join(Scotland_Population, Eave_Population) %>% 
  mutate(eave_weight=round(pop/pop_eave, 4)) %>% 
  mutate(eave_weight = ifelse(eave_weight >= 1, 1, eave_weight))

ggplot(df.w, aes(x=age)) + geom_line(aes(y=pop)) + geom_line(aes(y=pop_eave), colour="blue")+
  facet_wrap(~sex)

EAVE_weights <-  df %>% mutate(age=ifelse(Age>= 90, 90, Age)) %>% 
  left_join(select(df.w, age,sex,eave_weight), by=c("age"="age", "Sex"="sex" )) %>% 
  dplyr::select(EAVE_LINKNO, eave_weight)
str(EAVE_weights)



###

#EAVE_weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
df <- df %>%
  left_join(EAVE_weights, by="EAVE_LINKNO")




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
#table(df$Q_ETHNICITY_0)
df <-  df %>% mutate(any_comorb = select(., Q_BMI:SG_IMMRX_COD) %>% rowSums(na.rm = TRUE))
df <-  df %>% mutate(any_ethnicity = select(., Q_ETHNICITY_1:Q_ETHNICITY_9) %>% rowSums(na.rm = TRUE))
#table(df$any_ethnicity) # 74 duplicates

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
df <- df %>% mutate(agegrp = cut(Age, breaks = c(-1,18,29,39,49,59,69, 79, 89,121), 
                                 labels = c("00-18", "19-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-80", "90+")))

##SIMD
##get decile and rank as well as quintile
df$SIMDrank <-NULL
df$SIMD10   <-NULL           
df$hb2019name.y<-NULL
df$simd2020v2_rank<-NULL
df$simd2020v2_sc_decile<-NULL
df$SIMD5 <-NULL
df$simd2020v2_sc_quintile <-NULL
simd <- readRDS("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2020v2.rds")
#names(simd)
simd <- simd %>% dplyr::select(datazone2011, hb2019name,simd2020v2_rank, simd2020v2_sc_decile, simd2020v2_sc_quintile )
df <- left_join(df, simd, by = "datazone2011")
#table(df$simd2020v2_sc_decile)
df <- df %>% rename(SIMD5 = simd2020v2_sc_quintile)
df <- df %>% rename(SIMD10 = simd2020v2_sc_decile)
df <- df %>% rename(SIMDrank = simd2020v2_rank)

df$SIMD5 <- ifelse(is.na(df$SIMD5), "9 Not known",  df$SIMD5)
df$SIMD10 <- ifelse(is.na(df$SIMD10), "99 Not known",  df$SIMD10)
df$hb2019name <- ifelse(is.na(df$hb2019name), df$hb2019name.x,  df$hb2019name)
df$hb2019name.x <-NULL

df$SIMD5 <- ordered(df$SIMD5, levels = c("1", "2", "3", "4", "5", "9 Not known"))
df$SIMD10 <- ordered(df$SIMD10, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "99 Not known"))
str(df$SIMDrank)


##Other ctegorites
#table(df$Q_HOME_CAT, df$agegrp)
#table(df$Q_LEARN_CAT)

##filter on age####
#over 18s only!
table(df$agegrp)
table(as.numeric(QCovid_cohort$Age))
df <- df %>% filter(Age >18)
saveRDS(df, paste0(Location,"EAVE/GPanalysis/QCovid_Validation/data/Qcovid_prepped.rds"))
###Decriptive tables ----


#z.var <- "Q_DIAG_AF"
#z.df <- select(df, Age, Sex, starts_with(z.var)&ends_with(z.var), eave_weight) %>% rename(Var=3) %>% 
#  group_by(Age, Sex) %>% summarise(N=round(sum(eave_weight)), 
#                                       R=round(sum((Var==1)*eave_weight))) %>% 
#  mutate(Percent = round(R/N*100,1)) %>% filter(Age <=90)#
#
#g0 <- ggplot(z.df, aes(x=Age, y=Percent, col=Sex)) + geom_point() + geom_smooth(span=0.4) +
#  labs(x="Age", title=z.var) #

#g0+ labs(x="Age", title=z.var) 

##tables#################
###Comorbidities table
vars <- names(df)[grepl("Q_",names(df) )]
vars <- c(vars, "CKD_level" )
vars <- vars[!grepl("Q_DIAG_CKD", vars)]
#z.var.list[grepl("^Q", z.var.list) | grepl("^SG", z.var.list)| grepl("BMI", z.var.list)]
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft <- as.data.frame(do.call(rbind, freq.table.func))
ft <- ft %>% select(c(Variable, Level, Number, Percent)) # %>% mutate_all(as.character)
ft

# weighted numbers
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- df %>% group_by_at(x) %>% summarise(weighted_N = round_half_up(sum(eave_weight)))
  Percent <- round_half_up(prop.table(Number[["weighted_N"]])*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1 <- tb1 %>% rename(Level = x)
  tb1
})

ft.w <- as.data.frame(do.call(rbind, freq.table.func))

ft.w <- ft.w %>% select(c(Variable, Level, weighted_N, Percent)) # %>% mutate_all(as.character)
ft.w


# hosp_covid
df.h <- subset(df, hosp_covid==1)
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df.h[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft.h <- as.data.frame(do.call(rbind, freq.table.func))
ft.h <- ft.h %>% select(c(Variable, Level, Number, Percent))
ft.h

# death_covid
df.d <- subset(df, death_covid==1)
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df.d[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft.d <- as.data.frame(do.call(rbind, freq.table.func))
ft.d <- ft.d %>% select(c(Variable, Level, Number, Percent))
ft.d

names(ft.d) <- c("Variable",  "Level",  "Number_d",  "Percent_d")
##combine tables
ft <- left_join(ft, ft.w,   by=c("Variable", "Level"), all=T)
ft <- left_join(ft, ft.h,   by=c("Variable", "Level"), all=T)
ft <- left_join(ft, ft.d,   by=c("Variable", "Level"), all=T)
ft
ft <- flextable::as_grouped_data(x = ft, groups = c("Variable"))
row.names(ft) <- NULL # required to remove rownames when rendering to table in rmd
names(ft) <- c("Variable", "Level", "Number", "Percent", "Weighted Number", "Weighted Percent", "Number", "Percent",  "Number", "Percent")
ft


ft %>%
  kableExtra::kbl(caption = "Table 1b: Frequency and percent of cases and hospitalisations by comorbidity risk factor level") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria") %>%
  kableExtra::add_header_above(c(" " = 2, "Cases" = 2, "Weighted cases" = 2,  "Hospitalisations" = 2, "Deaths" = 2))

#saveRDS(ft, "/conf/EAVE/GPanalysis/outputs/temp/Qcovid_descr_tab1.rds")


###############################
###Comorbidities table
vars <- c("agegrp", "Sex",  "ethnic_grp",  "hb2019name" )

#"SIMD", "SIMD10",  - need to sort SIMD - losts missingm think matching ahs gonewrong somewhere.
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft <- as.data.frame(do.call(rbind, freq.table.func))
ft <- ft %>% select(c(Variable, Level, Number, Percent)) # %>% mutate_all(as.character)
ft
# weighted numbers
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- df %>% group_by_at(x) %>% summarise(weighted_N = round_half_up(sum(eave_weight)))
  Percent <- round_half_up(prop.table(Number[["weighted_N"]])*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1 <- tb1 %>% rename(Level = x)
  tb1
})

ft.w <- as.data.frame(do.call(rbind, freq.table.func))

ft.w <- ft.w %>% select(c(Variable, Level, weighted_N, Percent)) # %>% mutate_all(as.character)
ft.w
# hosp_covid
df.h <- subset(df, hosp_covid==1)
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df.h[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft.h <- as.data.frame(do.call(rbind, freq.table.func))
ft.h <- ft.h %>% select(c(Variable, Level, Number, Percent))
ft.h

# death_covid
df.d <- subset(df, death_covid==1)
freq.table.func <- lapply(vars, function(x) {
  # x <- "Sex"
  Number <- table(df.d[[x]])
  Percent <- round_half_up(prop.table(Number)*100, 1)
  tb1 <- cbind(Number, Percent)
  tb1 <- as.data.frame(tb1)
  tb1$Variable <- x
  tb1$Level <- rownames(tb1)
  tb1
})

ft.d <- as.data.frame(do.call(rbind, freq.table.func))
ft.d <- ft.d %>% select(c(Variable, Level, Number, Percent))
ft.d

#names(ft.d) <- c("Variable",  "Level",  "Number_d",  "Percent_d")
##combine tables
ft <- left_join(ft, ft.w,   by=c("Variable", "Level"), all=T)
ft <- left_join(ft, ft.h,   by=c("Variable", "Level"), all=T)
ft <- left_join(ft, ft.d,   by=c("Variable", "Level"), all=T)
ft
ft <- flextable::as_grouped_data(x = ft, groups = c("Variable"))
row.names(ft) <- NULL # required to remove rownames when rendering to table in rmd
names(ft) <- c("Variable", "Level", "Number", "Percent", "Weighted Number", "Weighted Percent", "Number", "Percent",  "Number", "Percent")
ft


ft %>%
  kableExtra::kbl(caption = "Table 1b: Frequency and percent of cases and hospitalisations by comorbidity risk factor level") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria") %>%
  kableExtra::add_header_above(c(" " = 2, "N persion" = 2, "Weighted cases" = 2,  "Hospitalisations" = 2, "Deaths" = 2))

saveRDS(ft, "/conf/EAVE/GPanalysis/outputs/temp/Qcovid_descr_tab2.rds")

