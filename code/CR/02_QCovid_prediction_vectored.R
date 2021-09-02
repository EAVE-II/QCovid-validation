###################################################################### 

## Title: External validation of the QCOVID risk prediction algorithm to estimate future risk of COVID-19 hospitalisation and death: 
##        a prospective cohort study 

## Short title: QCovid validation

## DOI: 

## Code author: Steven Kerr

## Description: This code provides functions that calculate the probability of death and hospitalisation according to the algorithm
##              here: https://qcovid.org/Home/Algorithm

###################################################################### 





# The four functions in this code all take as an argument an n x 40 matrix where each column is an attribute. The following is an orderer 
# list of the columns in x. I have added spaces to indicate different variable "groupings". Further information on the variables can be found in
# the file "Qcovid core clusters".
# 
#   1. age
#   2. bmi
#   3. town
#   4. surv
#   
#   5. chemocat
#   6. ethrisk
#   7. homecat
#   8. learncat
#   9. renalcat
#   
#   10. b2_82
#   11. b2_leukolaba
#   12. b2_prednisone
#   13. b_AF
#   14. b_CCF
#   15. b_asthma
#   16. b_bloodcancer
#   17. b_cerebralpalsay
#   18. b_chd
#   19. b_cirrhosis
#   20. b_congenheart
#   21. b_copd
#   22. b_dementia
#   23. b_epilepsy
#   24. b_fracture4
#   25. b_neurorare
#   26. b_parkinsons
#   27. b_pulmhyper
#   28. b_pulmrare
#   29. b_pvd
#   30. b_ra_sle
#   31. b_respcancer
#   32. b_semi
#   33. b_sicklecelldisease
#   34. b_stroke
#   35. b_type1
#   36. b_type2
#   37. b_vte
#   
#   38. p_marrow6
#   39. p_radio6
#   40. p_solidtransport





# death_female is a function that predicts the probability of death of females.

death_female <- function(x){
  
  # survivor is a 91-dimensional vector whose nth element is the probability of surviving until day n for the baseline group.
  # At the moment, it is all zeroes except for the final entry, which is not strictly how it should be.
  # However, currently it is only being used to predict the probability of survival until day 91, so only the last entry is ever used.
  # I have coded it similarly to the algorithm as found on https://qcovid.org/Home/Algorithm in case this changes.
  
  survivor <- numeric(91) 
  
  survivor[91] <- 0.999977290630341
  survivor[60] <- 0.999985026731318  
  
  # Ichemocat, iethrisk, Ihomecat, Ilearncat and Irenalcat each contain estimated parameter values corresponding to categorical variables
  # that can take on multiple values.
  
  Ichemocat <- c(0, 0.8345234517964427167768804, 
                 1.2595202625466794810193960, 
                 2.8514552483447963560081462) 
  
  Iethrisk <- c( 0,
                 0,
                 0.6389369130057840351355480,
                 0.3342933389380337572127644,
                 0.3428258158976604796919219,
                 0.1716307703741346002423995,
                 0.5199930351630326352818656,
                 0.6823609168626041387994974,
                 0.1930811621976745162676536,
                 0.5483388756920363205082936)
  
  Ihomecat <- c(0,
                1.2843897452012438265001038,
                0.3902468934897107555315188)
  
  Ilearncat <- c(0,
                 0.3039596464330543423848496,
                 3.4826484000729771572935078)
  
  Irenalcat <- c(0,
                 0,
                 0.2651025698152708609534045,
                 0.3172050204046410470937190,
                 1.0983587266864589526704776,
                 0.9870752383405548835426657,
                 2.0594912500810331756895266)
  
  
  # dage, age_1, age_2, bmi_1, bmi-2 and town are all new variables derived from elements of x
  
  dage <- x[,1]/10
  
  age_1 <- dage^3 - 115.599884033203125
  
  age_2 <- (dage^3) * log(dage) - 183.038345336914062
  
  dbmi <- x[,2]/10
  
  bmi_1 <- dbmi^0.5 - 1.632479429244995
  
  bmi_2 <- (dbmi^0.5) * log(dbmi) - 1.600156426429749
  
  
  town <- x[,3] - 0.327639788389206
  
  # coef is a vector of coefficients for variables x[10:40]
  
  coef <- c(0.0859851843797995313289917,
            0.2037201166969377086335413,
            0.6019896780418610982010819,
            0.1660328711606123830435422,
            0.3158788199551698649969467,
            -0.1708080810500710866595142,
            0.4045790554089616630761839,
            1.2375891121070539124815468,
            0.2178442812641575854204490,
            0.6165061556893419725255967,
            0.2099695358513284648704911,
            0.4069036991923689616790227,
            1.0673649341196249640262295,
            0.4563898846837706191337247,
            0.1142356737251236653563069,
            1.0103599800977995926132280,
            0.1256622652011334939636811,
            0.4365766436191424459956067,
            -0.1665545651399719662144605,
            0.3521144005866303494656222,
            0.2775017265662046983543121,
            0.5288182839278391389470357,
            0.2549134429838964543968416,
            1.7818709562115360167666722,
            0.2915465900033972213023503,
            1.3918300744178950800744587,
            1.8389652399973426266654997,
            0.1657707292848709101917848,
            1.0220219382440418609547805,
            0.7446485607185400201757375,
            0.3787854828143376040294754)
  
  
  # score is the predicted probability as a percentage.
  # a is used in calculating score.
  
  a <- Ichemocat[ x[,5] ]  + Iethrisk[ x[,6] ] + Ihomecat[ x[,7] ] + Ilearncat[ x[,8] ] + Irenalcat[ x[,9] ]
  
  a <- a + 0.0535266800950749549459218 * age_1 - 0.0200935878258154260178614 * age_2 - 19.7435582245984164728724863 * bmi_1 +
    6.6648702078668167203545636 * bmi_2 + 0.0787269477751315061020421 * town
  
  a <- a + (as.matrix(x[,10:40]) %*% coef ) - 0.0200621605517602719093162 * age_1 * x[,36] + 0.0074957790032429043661222 * age_2 * x[,36]
  
  score <- 100.0 * (1 -  exp( exp(a) * log( survivor[ x[,4] ] )  )  )
  
  return(list(score, a))
}







# hospital_female is a function that predicts the probability of hospitalisation females.

hospital_female <- function(x){
  
  # survivor is a 91-dimensional vector whose nth element is the probability of surviving until day n for the baseline group.
  # At the moment, it is all zeroes except for the final entry, which is not strictly how it should be.
  # However, currently it is only being used to predict the probability of survival until day 91, so only the last entry is ever used.
  # I have coded it similarly to the algorithm as found on https://qcovid.org/Home/Algorithm in case this changes.
  
  survivor <- numeric(91) 
  
  survivor[91] <- 0.999614596366882
  survivor[60] <- 0.999745871030182 
  
  # Ichemocat, iethrisk, Ihomecat, Ilearncat and Irenalcat each contain estimated parameter values corresponding to categorical variables
  # that can take on multiple values.
  
  Ichemocat <- c(0,
                 0.7468184809678632962715028,
                 1.4335568622103449509808115,
                 2.7425623418828077859643599) 
  
  
  Iethrisk <- c( 0,
                 0,
                 0.6381416181144948795989080,
                 0.4154336125176559812999244,
                 0.3460373664440339336323404,
                 0.7629835412388623616664063,
                 0.6956993818885366387405611,
                 0.8317012026776886557399848,
                 0.1388269618785758774404115,
                 0.6432491845097410010367867)
  
  
  Ihomecat <- c(0,
                0.6111109399113108242573844,
                0.2046773133743576833509792)
  
  
  Ilearncat <- c(0,
                 0.4267124890741003651051244,
                 2.1792421003277180346913156)
  
  
  Irenalcat <- c(0,
                 0,
                 0.2982583643377000881535821,
                 0.5848878422963580403504125,
                 1.4268609433854653190110184,
                 1.3148983941801617447708850,
                 1.7128356862419007455855535)
  
  # dage, age_1, age_2, bmi_1, bmi-2 and town are all new variables derived from elements of x
  
  dage <- x[,1]/10
  
  age_1 <- dage^0.5 - 2.207121372222900
  
  age_2 <- dage^2 - 23.730392456054688
  
  dbmi <- x[,2]/10
  
  bmi_1 <- dbmi^(-2) - 0.140802085399628
  
  bmi_2 <- log(dbmi) - 0.980199992656708
  
  
  town <- x[,3] - 0.327639788389206
  
  
  # coef is a vector of coefficients for variables x[10:40]
  
  coef <- c(0.2742900245435872519372822,
            0.2676801383148893487273767,
            0.6545953126312467063030454,
            0.2903016741277205103877179,
            0.3236173092786057137182354,
            0.1121787272583002481596282,
            0.3377863143614412422266469,
            0.9778476962006689143791505,
            0.1076997815345036024758940,
            0.6047183940676497115873644,
            0.1825617255143640038639319,
            0.2940790450123081933853086,
            0.5469427350294541190223185,
            0.4527175076655778340750658,
            0.2983585791165178080497355,
            0.9059625435491620581984762,
            0.5286796359462073713331165,
            0.4692582807440949799193675,
            0.2499579320244159075237178,
            0.1942006598368692937839342,
            0.3019421121979438682458863,
            0.4999823548811314077866541,
            0.3162439787347626762858965,
            1.8985404911409835548852243,
            0.3305273237679022813040319,
            1.3943531760373193417734683,
            0.9708326823437052333076736,
            0.2955542808178847624667185,
            0.1893906144867409657717161,
            0.3876198174592248579806153,
            0.4490079929764763111421644)
  
  # score is the predicted probability as a percentage.
  # a is used in calculating score.
  
  a <- Ichemocat[ x[,5] ]  + Iethrisk[ x[,6] ] + Ihomecat[ x[,7] ] + Ilearncat[ x[,8] ] + Irenalcat[ x[,9] ]
  
  a <- a  -0.1484733673762321515265938 * age_1 + 0.0405941535676193412940371 * age_2 + 6.1144623880326562925802136 * bmi_1 +
    2.7351660262730592698687815 * bmi_2 + 0.0837552324383479818159515 * town
  
  
  a <- a + (as.matrix(x[,10:40]) %*% coef ) - 1.1514860942738034399468461 * age_1 * x[,36] + 0.0018396028070442396740169 * age_2 * x[,36]
  
  score <- 100.0 * (1 -  exp( exp(a) * log( survivor[ x[,4] ] )  )  )
  
  return(list(score, a))
}






# death_male is a function that predicts the probability of death of a males.

death_male <- function(x){
  
  # survivor is a 91-dimensional vector whose nth element is the probability of surviving until day n for the baseline group.
  # At the moment, it is all zeroes except for the final entry, which is not strictly how it should be.
  # However, currently it is only being used to predict the probability of survival until day 91, so only the last entry is ever used.
  # I have coded it similarly to the algorithm as found on https://qcovid.org/Home/Algorithm in case this changes.
  
  survivor <- numeric(91) 
  
  survivor[91] <- 0.999956965446472
  survivor[60] <- 0.999971625361112 
  
   
  # Ichemocat, iethrisk, Ihomecat, Ilearncat and Irenalcat each contain estimated parameter values corresponding to categorical variables
  # that can take on multiple values.
  
  Ichemocat <- c(0,
                 0.5518382023402614855456250,
                 1.2521892539965664425949399,
                 1.2136983258866618218263511) 
  
  Iethrisk <- c( 0,
                 0,
                 0.4612255535316009957824690,
                 0.6111266423098750122377965,
                 0.8204730544167745387440505,
                 0.7014941739425393230078498,
                 0.7240940102093914587655377,
                 1.1094023093836316018467869,
                 0.9042387414289448921422832,
                 0.7111426262631623806953485)
  
  Ihomecat <- c(0,
                1.4544904405508778388878000,
                0.4426122079730733793745401)
  
  Ilearncat <- c(0,
                 0.3038620602034622364406857,
                 2.2820367212000198797738904)
  
  
  Irenalcat <- c(0,
                 0,
                 0.1624549376621728935532474,
                 0.6050038962863354408128203,
                 0.8751467115376150296413016,
                 1.2993052354424052818870905,
                 1.1624518358657724981242154)
  
  # dage, age_1, age_2, bmi_1, bmi-2 and town are all new variables derived from elements of x
  
  dage <- x[,1]/10
  
  age_1 <- dage - 4.770704746246338
  
  age_2 <- dage^3 - 108.579444885253906
  
  dbmi <- x[,2]/10
  
  bmi_1 <- dbmi^(-0.5) - 0.613678753376007
  
  bmi_2 <- (dbmi^(-0.5)) * log(dbmi)  - 0.599298655986786
  
  
  town <- x[,3] - 0.436863929033279
  
  
  # coef is a vector of coefficients for variables x[10:40]
  
  coef <- c(0.4591322211314322609965188,
            0.0412503496956075910162554,
            0.3626076782717908564279696,
            0.1052111096824691061080159,
            0.3396973581366389827174146,
            0.0321078769888719015024314,
            0.2516787141016778583946234,
            1.0196367443019023202310791,
            0.1185398586465893217001266,
            0.2579778953047697220846146,
            0.0297033412293931098346889,
            0.2247771466899423364882438,
            1.1427890629158352631122852,
            0.4706338398323707261639015,
            0.3447414553384527402535298,
            0.6862474948256528373136121,
            0.6584837103149563386494947,
            0.3845567043397818407513000,
            0.1846607906556500255934594,
            0.3234029467753249398320747,
            0.0152761200100454391098692,
            0.2405175355951279514421515,
            0.2342064563503280238965232,
            1.4829018816463233054747661,
            0.2126223941662605809721498,
            1.7655772899003154829955520,
            1.5551685093805573956160515,
            0.3075001687704595476624547,
            1.8089884245023548636766009,
            0.7391718905778561499175794,
            0.5444438235973578787962879)
  
  # score is the predicted probability as a percentage.
  # a is used in calculating score. 
  
  a <- Ichemocat[ x[,5] ]  + Iethrisk[ x[,6] ] + Ihomecat[ x[,7] ] + Ilearncat[ x[,8] ] + Irenalcat[ x[,9] ]
  
  a <- a + 1.4547532388624053734105246 * age_1 - 0.0028280265297625597521736 * age_2 - 22.0609948165960112476113864 * bmi_1 -
    20.3035078034241216471400548 * bmi_2  + 0.0812689785693790078813237 * town
  
  
  a <- a + (as.matrix(x[,10:40]) %*% coef) - 0.5325370730252168005591784 * age_1 * x[,36] + 0.0013434948852218797209906 * age_2 * x[,36]
  
  score <- 100.0 * (1 -  exp( exp(a) * log( survivor[ x[,4] ] )  )  ) 
  
  return(list(score, a))
}







# hospital_male is a function that predicts the probability of hospitalisation of a males.

hospital_male <- function(x){
  
  # survivor is a 91-dimensional vector whose nth element is the probability of surviving until day n for the baseline group.
  # At the moment, it is all zeroes except for the final entry, which is not strictly how it should be.
  # However, currently it is only being used to predict the probability of survival until day 91, so only the last entry is ever used.
  # I have coded it similarly to the algorithm as found on https://qcovid.org/Home/Algorithm in case this changes.
  
  survivor <- numeric(91) 
  
  survivor[91] <- 0.999484241008759
  survivor[60] <- 0.999659909245739 
  
  # Ichemocat, iethrisk, Ihomecat, Ilearncat and Irenalcat each contain estimated parameter values corresponding to categorical variables
  # that can take on multiple values.
  
  Ichemocat <- c(0,
                 0.5396791056031334798959165,
                 1.2925251045919026182673406,
                 1.4127304180622570761727275) 
  
  
  Iethrisk <- c( 0,
                 0,
                 0.7634218127621078542333066,
                 0.7004223248170889926100813,
                 0.5371036969721910692143751,
                 0.8272743507914643856793191,
                 0.8264788057106159380182930,
                 0.9530259006923202935723793,
                 0.4095851796464533567387889,
                 0.7509020278385331037540595)
  
  
  Ihomecat <- c(0,
                0.9242505123586811643932037,
                0.4041900688356840354309440)
  
  
  Ilearncat <- c(0,
                 0.3227329968905671697676496,
                 1.4717734162553066656897727)
  
  
  Irenalcat <- c(0,
                 0,
                 0.2486111847569784372158352,
                 0.6922251739537846892957873,
                 1.3499829137108374510489739,
                 1.7747394666416362873917478,
                 1.9583171284266656098083104)
  
  # dage, age_1, age_2, bmi_1, bmi-2 and town are all new variables derived from elements of x
  
  dage <- x[,1]/10
  
  age_1 <- dage^(-2) - 0.043937455862761
  
  age_2 <- dage^2 - 22.759624481201172
  
  dbmi <- x[,2]/10
  
  bmi_1 <- dbmi^(-0.5) - 0.613678753376007
  
  bmi_2 <- log(dbmi) - 0.976567327976227
  
  
  town <- x[,3] - 0.436863929033279
  
  
  # coef is a vector of coefficients for variables x[10:40]
  
  coef <- c(0.1096448643067308714726948,
            0.1684284444365026223167092,
            0.3541104309369558778008980,
            0.1758375829207621254823835,
            0.2837377731487550902578221,
            0.0967382412217892284633791,
            0.2509843520785098314185291,
            1.0462838419104085740940491,
            0.0610268853855645346251357,
            0.6299545424034566076798569,
            -0.0319579062821411449824005,
            0.3083628647668430478034907,
            0.7510502350454668629708976,
            0.5603168206085185065745691,
            0.3019787013219903393235199,
            1.2049102841347989478748559,
            0.7178953912544794313532748,
            0.4420231036633441856764648,
            0.2519365241136610200634038,
            0.2362081915599513470827020,
            0.2597912276586239466169559,
            0.3673198175857035341351775,
            0.2479742845321690758453315,
            1.5829020197071794751053631,
            0.2673711337366190821107637,
            1.2986534162809348913469876,
            0.9444989479513733465765313,
            0.2608403897792810899325389,
            0.5296880469252451817396832,
            0.7018887794605082053323031,
            0.7032493213195643022572767)
  
  # score is the predicted probability as a percentage.
  # a is used in calculating score. 
  
  a <- Ichemocat[ x[,5] ]  + Iethrisk[ x[,6] ] + Ihomecat[ x[,7] ] + Ilearncat[ x[,8] ] + Irenalcat[ x[,9] ]
  
  a <- a -9.8655000090771149956481167 * age_1 + 0.0372128338797922050829037 * age_2 + 21.1033159148484443790039222 * bmi_1 +
    7.4762210517919633900874032 * bmi_2 + 0.0763068123197961217796248 * town
  
  
  a <- a + (as.matrix(x[,10:40]) %*% coef) + 8.1824740477927431214766330 * age_1 * x[,36] - 0.0088155777714664287220137 * age_2 * x[,36]
  
  score <- 100.0 * (1 -  exp( exp(a) * log( survivor[ x[,4] ] )  )  )
  
  return(list(score, a))
}

