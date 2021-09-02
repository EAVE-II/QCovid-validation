library(dplyr)
QCovid_1 <- readRDS("/conf/EAVE/GPanalysis/data/QCOVIDdeduped.rds")
QCovid_2 <- readRDS("/conf/EAVE/GPanalysis/data/cleaned_data/Qcovid_update_Jan21.rds")


#EAVEQ2 <- unique(QCovid_2$EAVE_LINKNO)
#sum(EAVEQ2 %in% QCovid_1$EAVE_LINKNO)


##Who do we not have in Q2? Records from Q1 should be kept for these people.

Q1 <- QCovid_1 %>% filter(!EAVE_LINKNO %in% EAVEQ2) 
rm(QCovid_1)

Qcovid_2 <- QCovid_2 %>% dplyr::select(EAVE_LINKNO, cluster,Value, EventDate) %>%
  arrange(EAVE_LINKNO, cluster, EventDate) %>% group_by(EAVE_LINKNO, cluster)  %>%
  slice(1) %>%
  ungroup %>%
  select(-EventDate)

#table(!is.na(Qcovid_2$EAVE_LINKNO))
#Qsample <- Qcovid_2[1:1000,]
data_wide <- tidyr::spread(Qcovid_2, cluster, Value)

names(Q1)
names(data_wide)

Q1 <- Q1 %>% mutate(Q_ETHNICITY = case_when(Q_ETHNICITY_0 ==1 ~0, 
                                            Q_ETHNICITY_1 ==1 ~1,
                                            Q_ETHNICITY_2==1 ~2,
                                            Q_ETHNICITY_3 ==1 ~3,
                                            Q_ETHNICITY_4 ==1 ~4,
                                            Q_ETHNICITY_5 ==1 ~5,
                                            Q_ETHNICITY_6 ==1 ~6,
                                            Q_ETHNICITY_7 ==1 ~7,
                                            Q_ETHNICITY_8 ==1 ~8,
                                            Q_ETHNICITY_9 ==1 ~9,
                                            Q_ETHNICITY_10 ==1 ~10,
                                            Q_ETHNICITY_11 ==1 ~11,
                                            Q_ETHNICITY_12 ==1 ~12,
                                            Q_ETHNICITY_13 ==1 ~13,
                                            Q_ETHNICITY_14 ==1 ~14,
                                            Q_ETHNICITY_15 ==1 ~15,
                                            Q_ETHNICITY_16 ==1 ~16, 
                                            TRUE ~0)) %>%
  mutate(Q_DIAG_CKD_LEVEL = case_when(Q_DIAG_CKD3==1~ 3, 
                                      Q_DIAG_CKD4==1~ 4, 
                                      Q_DIAG_CKD5==1~ 5,
                                      TRUE~0)) %>%
  select(-c(Q_DIAG_CKD3, Q_DIAG_CKD4, Q_DIAG_CKD5, Q_ETHNICITY_0, Q_ETHNICITY_1, Q_ETHNICITY_2, Q_ETHNICITY_3, Q_ETHNICITY_4,
            Q_ETHNICITY_5, Q_ETHNICITY_6, Q_ETHNICITY_7, Q_ETHNICITY_8, Q_ETHNICITY_9, Q_ETHNICITY_10, Q_ETHNICITY_11, 
            Q_ETHNICITY_12, Q_ETHNICITY_13, Q_ETHNICITY_14, Q_ETHNICITY_15, Q_ETHNICITY_16))


names(Q1)
names(data_wide)

EAVE_link <- readRDS("/conf/EAVE/GPextract/data/MASTER_id_file.rds")
EAVE_link <- EAVE_link %>% dplyr::select(EAVE_LINKNO, CHINumber, UPI_NUMBER)

Q1_demog <- Q1 %>% select(c("EAVE_LINKNO", "Sex" , "Age", "datazone2011", "simd2020v2_sc_quintile", "DeductionDate"))
Q1 <- Q1 %>% select(-c("Sex" , "Age", "datazone2011", "simd2020v2_sc_quintile", "DeductionDate"))
Q1 <- Q1 %>% select(-c( "Q_RX_IMMUNO", "Q_RX_LABA", "Q_RX_PRED" , "SG_ASTRX_COD" , "SG_IMMRX_COD"  ))

Q1 <-Q1 %>% rename("Q_BMI" = "BMI")
Q_all <- rbind(Q1, data_wide)

Q_all <- left_join(Q_all, EAVE_link)
table(is.na(Q_all$CHINumber))

names(Q1_demog)
postcode <- readRDS("/conf/EAVE/GPanalysis/data/postcode_to_dz_iz_hb_ca_simd.rds")
UR <- haven::read_sav("/conf/EAVE/GPanalysis/data/lookups/datazone2011_urban_rural_2016.sav")
str(postcode)
postcode$pc7 <- phsmethods::postcode(postcode$post_code, "pc7" )
postcode <- postcode %>% select(pc7, hb2019name, simd2020)

UR <- UR %>% select(Datazone2011, UR8_2016, UR6_2016)

Q2_demog <- QCovid_2 %>% select(EAVE_LINKNO, Age, Sex, simd2020, hb2019name )
