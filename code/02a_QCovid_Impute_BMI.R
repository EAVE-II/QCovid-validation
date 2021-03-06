#Calculate the impute values for bmi


z_miss_bmi <- filter(df, is.na(BMI)) %>% 
  dplyr::select(Age, Sex, town, Q_DIAG_AF:Q_DIAG_VTE, Q_HOME_CAT, Q_LEARN_CAT, CKD_level, EAVE_LINKNO) %>% 
  dplyr::select(-(Q_DIAG_CKD3:Q_DIAG_CKD5)) %>% 
  mutate(Sex=if_else(Sex=="M",1,0))

pred_bmi_coefs <- c(25.454589211,  0.034819271, -0.200758681,  0.078336103,  0.349318439,
    0.935957507, -0.676673763, 0.206517652, -0.831372977,  0.215348398,  0.205422595,
    -0.037882696, -1.186753441, -2.815319705, 0.371056996,  3.423302834,  0.082725341,
    -0.994328752, -0.432617520, -1.314852087, -0.667638044, -1.247299417, -1.348456939,
    -0.005665688, -1.775427465,  0.886037579, -0.595863315, -0.521845996,
    1.039846838, -1.171865402,  0.743896428, -0.100554399, -0.375900747, -0.386150319)
names(pred_bmi_coefs) <- 
c("(Intercept)",          "Age",                  "SexM",                 "town",                
"Q_DIAG_AF",            "Q_DIAG_ASTHMA",        "Q_DIAG_BLOOD_CANCER",  "Q_DIAG_CCF"  ,        
"Q_DIAG_CEREBRALPALSY", "Q_DIAG_CHD"   ,        "Q_DIAG_CIRRHOSIS",     "Q_DIAG_CONGEN_HD" ,   
"Q_DIAG_COPD",          "Q_DIAG_DEMENTIA",      "Q_DIAG_DIABETES_1",    "Q_DIAG_DIABETES_2",   
"Q_DIAG_EPILEPSY",      "Q_DIAG_FRACTURE",      "Q_DIAG_NEURO",         "Q_DIAG_PARKINSONS",   
"Q_DIAG_PULM_HYPER",    "Q_DIAG_PULM_RARE",     "Q_DIAG_PVD" ,          "Q_DIAG_RA_SLE",       
"Q_DIAG_RESP_CANCER",   "Q_DIAG_SEV_MENT_ILL",  "Q_DIAG_SICKLE_CELL",   "Q_DIAG_STROKE",       
"Q_DIAG_VTE" ,          "Q_HOME_CAT",           "Q_LEARN_CAT" ,         "CKD_levelCKD 3" ,     
"CKD_levelCKD 4" ,      "CKD_levelCKD 5") 
z_pred <- as.matrix(z_miss_bmi[, 1:30]) %*% pred_bmi_coefs[2:31]
z_pred <- z_pred + pred_bmi_coefs[1] + (z_miss_bmi[,31]== "CKD 3")*pred_bmi_coefs[32] +
  (z_miss_bmi[,31]== "CKD 4")*pred_bmi_coefs[33] +
  (z_miss_bmi[,31]== "CKD 5")*pred_bmi_coefs[34]
z_pred <- as.numeric(z_pred[,1])
z_pred <- data.frame(EAVE_LINKNO = z_miss_bmi$EAVE_LINKNO, bmi_impute=z_pred)

df <- df %>%  left_join(z_pred, by="EAVE_LINKNO")
  
#code used to slect the model
#z_mod <- lm(BMI ~ ., data=z_df)
#summary(z_mod)
#z <- step(z_mod, direction="both", k=2)

#z_newdata <- filter(df, is.na(BMI))
#z_pred <- predict(z, newdata=z_newdata
#z_newdata$pred_regr <- z_pred


