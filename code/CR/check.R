z.names <- c("Age"    ,              "BMI"   ,               "town",                
"surv"  ,               "chemocat" ,            "ethrisk" ,             "Q_HOME_CAT" ,          "Q_LEARN_CAT" ,        
"renalcat",             "Q_RX_IMMUNO" ,         "Q_RX_LABA",            "Q_RX_PRED" ,           "Q_DIAG_AF" ,          
 "Q_DIAG_CCF" ,          "Q_DIAG_ASTHMA",       "Q_DIAG_BLOOD_CANCER",  "Q_DIAG_CEREBRALPALSY", "Q_DIAG_CHD" ,         
 "Q_DIAG_CIRRHOSIS",     "Q_DIAG_CONGEN_HD" ,    "Q_DIAG_COPD",          "Q_DIAG_DEMENTIA" ,     "Q_DIAG_EPILEPSY" ,    
 "Q_DIAG_FRACTURE" ,     "Q_DIAG_NEURO",         "Q_DIAG_PARKINSONS",    "Q_DIAG_PULM_HYPER" ,   "Q_DIAG_PULM_RARE",    
 "Q_DIAG_PVD"   ,        "Q_DIAG_RA_SLE",        "Q_DIAG_RESP_CANCER",   "Q_DIAG_SEV_MENT_ILL",  "Q_DIAG_SICKLE_CELL"  ,
 "Q_DIAG_STROKE" ,       "Q_DIAG_DIABETES_1" ,   "Q_DIAG_DIABETES_2",    "Q_DIAG_VTE" ,          "p_marrow6" ,          
 "p_radio6",             "p_solidtransplant")

x <- numeric(40)
x[1:4] <- c(23,24,-0.08316642,91)
x[5:9] <- c(1,1,1,1,1)

death_male(x)
hospital_female(x)
