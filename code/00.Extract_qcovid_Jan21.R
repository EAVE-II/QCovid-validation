##########################################################
# Name of file: Vaccine data extract 
# Data release (if applicable):
# Original author(s): Emily Moore emilymoore3@nhs.net
# Original date: 19 January 2021
# Latest update author (if not using version control)
# Latest update date (if not using version control)
# Latest update description (if not using version control)
# Type of script: Data prep & linkage
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content:  Downloads Qcovid update for January 2021. Strip chi, add eave linkno.
#                         Cleans to combine clusters with mutlple values (e.g. ethnicity and learncat)
# Approximate run time: Unknown
##########################################################

#01 - housekeeping ----

#packages
library(odbc)
library(dplyr)

#AOGROUP1LSTNR
TRANSFER <- dbConnect(odbc(), 
                      Driver = "SQLServer", 
                      Server = "AOGROUP1LSTNR", 
                      Database = "NSSTRANSFER", 
                      UID = "emilym02", 
                      PWD =  .rs.askForPassword("NSSTRANSFER Password:"))


#odbcListObjects(TRANSFER)
dbListTables(TRANSFER)
odbcPreviewObject(TRANSFER, table = "EAVE_QCOVID_OUT", rowLimit=1)
odbcPreviewObject(TRANSFER, table = "QcovidMarchBaselineV2tmp", rowLimit=1)
#odbcPreviewObject(TRANSFER, table = "EAVE_FLU_VACS", rowLimit=1)

# 2 data extract ----
# use select distinct to remove any duplication.
Qcovid <- dbGetQuery(TRANSFER,statement = 'SELECT DISTINCT 
                     CHINumber, Postcode, DateOfBirth, Sex, Age, cluster, EventDate, Value
                     FROM
                     EAVE_QCOVID_OUT')
str(Qcovid)

# 3 Check & clean CHI ----

Qcovid$CHINumber <- sprintf("%010.0f",Qcovid$CHINumber)
Qcovid <- Qcovid %>% mutate(validchi  = phsmethods::chi_check(CHINumber))
table(Qcovid$validchi)
Qcovid <- Qcovid %>% filter(validchi == "Valid CHI")

Qcovid <- Qcovid %>% arrange(CHINumber, cluster, EventDate)

#Seelct latest event for each cluster
Qcovid <- Qcovid %>% dplyr::select(-c(PatientKey,  RegDate, IsDeducted, DeductionDate) ) %>%
  arrange(CHINumber, cluster, EventDate) %>% group_by(CHINumber, cluster)  %>%
  slice(1) %>%
  ungroup 
##split binary variables and others.
#table(min$value, min$cluster)

learn <-  Qcovid %>% filter(cluster =="Q_LEARN_CAT_1" | cluster =="Q_LEARN_CAT_2") 
learn <- learn %>% mutate(Value= ifelse(cluster =="Q_LEARN_CAT_1",1,ifelse(cluster =="Q_LEARN_CAT_2",2,0))) %>%
  mutate(cluster ="Q_LEARN_CAT")


home <-  Qcovid %>% filter(cluster =="Q_HOME_CAT_1" | cluster =="Q_HOME_CAT_2") 
home <- home %>% mutate(Value= ifelse(cluster =="Q_HOME_CAT_1",1,ifelse(cluster =="Q_HOME_CAT_2",2,0))) %>%
  mutate(cluster ="Q_HOME_CAT")

bmi <- Qcovid %>% filter(cluster =="Q_BMI")

kidney <- Qcovid %>% filter(cluster =="Q_DIAG_CKD3" | cluster =="Q_DIAG_CKD4" | cluster == "Q_DIAG_CKD5")
kidney <- kidney %>% mutate(Value= case_when(cluster =="Q_DIAG_CKD3" ~ 3,
                                             cluster =="Q_DIAG_CKD4" ~ 4,
                                             cluster =="Q_DIAG_CKD5" ~ 5,
                                             TRUE ~ 0) )%>%
  mutate(cluster ="Q_DIAG_CKD_LEVEL")

##Ethnicity to be separate

ethnicity <- Qcovid %>% filter(stringr::str_detect(cluster, 'ETHNICITY')) %>% 
  mutate(Value  = readr::parse_number(cluster))

ethnicity <- ethnicity %>% mutate(cluster= "Q_ETHNICITY")

rx <- Qcovid %>% filter(stringr::str_detect(cluster, 'Q_RX_'))

min <- Qcovid %>% filter(!stringr::str_detect(cluster, 'ETHNICITY')) %>% 
  filter(!stringr::str_detect(cluster, 'CKD')) %>% filter(!stringr::str_detect(cluster, 'HOME')) %>% 
  filter(!stringr::str_detect(cluster, 'LEARN')) %>% filter(!stringr::str_detect(cluster, 'BMI')) %>% 
  filter(!stringr::str_detect(cluster, 'Q_RX_')) %>% mutate(Value=1)
#rm(min)
table(min$Value)
Qcovid <- rbind(min, kidney,home, learn, bmi, ethnicity)

#source("/conf/bss/01-Projects/01-Open-projects/XRB17112 - EAVE II/Functions_for_idfiles.R")
#add_chi_to_linkfile(Qcovid, chi = "CHINumber", source = "GP")

EAVE_link <- readRDS("/conf/EAVE/GPextract/data/MASTER_id_file.rds")
EAVE_link <- EAVE_link %>% dplyr::select(EAVE_LINKNO, CHINumber, UPI_NUMBER)

Qcovid <- left_join(Qcovid, EAVE_link, by = c("CHINumber" = "CHINumber"))

#join hb and simd
postcode <- readRDS("/conf/EAVE/GPanalysis/data/postcode_to_dz_iz_hb_ca_simd.rds")
UR <- haven::read_sav("/conf/EAVE/GPanalysis/data/lookups/datazone2011_urban_rural_2016.sav")

postcode$pc7 <- phsmethods::postcode(postcode$post_code, "pc7" )
postcode <- postcode %>% select(pc7, hb2019name, simd2020)

UR <- UR %>% select(Datazone2011, UR8_2016, UR6_2016)

#
Qcovid$pc7 <-  phsmethods::postcode(Qcovid$Postcode, "pc7" )

UR <- UR %>% select(Datazone2011, UR8_2016, UR6_2016)


Qcovid <- left_join(Qcovid, postcode)
wrongpc <- Qcovid %>% filter(is.na(pc7))

postcode <- readRDS("/conf/EAVE/GPanalysis/data/postcode_to_dz_iz_hb_ca_simd.rds")
postcode$pc7 <- phsmethods::postcode(postcode$post_code, "pc7" )

dz <- postcode %>% select(pc7, datazone2011)
Qcovid <- left_join(Qcovid, dz)
Qcovid <- left_join(Qcovid, UR, by = c("datazone2011" = "Datazone2011"))

Qcovid_anon <- Qcovid %>% select(EAVE_LINKNO, Age, Sex,  cluster, Value, EventDate, simd2020, hb2019name, UR8_2016, UR6_2016)
saveRDS(Qcovid_anon, "/conf/EAVE/GPanalysis/data/cleaned_data/Qcovid_update_Jan21.rds")
saveRDS(Qcovid, "/conf/EAVE/GPextract/data/Qcovid_update_Jan21.rds")


