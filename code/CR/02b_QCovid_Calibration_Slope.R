library(rms)

# Calibration slope and intercept calculation SRK
get_slope_intercept <- function(df){
 
  #add postscript to  all Qrisk in the mutate to _orig for the origial calculation - not the recalibration
  
  df <- mutate(df, logit_Qrisk_death = log( (Qrisk_death/100) / (1-(Qrisk_death/100) )),
               logit_Qrisk_hosp = log( (Qrisk_hosp_/100) / (1-(Qrisk_hosp/100) ))) 
  
  model <- glm(death_covid ~ logit_Qrisk_death , data=df, family="binomial") %>%
    summary()
  
  estimates <- format_estimates(model) %>% dplyr::rename(Death = result)
  
  model <- glm(hosp_covid ~ logit_Qrisk_hosp , data=df,  family="binomial", subset=hosp_remove==0) %>%
    summary()
  
  estimates <- cbind( estimates, format_estimates(model)%>% dplyr::rename(Hospitalisation = result))
  
  return(estimates)
}


format_estimates <- function(model){
  
  estimates <- model$coefficients[, 1:2] %>% as.data.frame()
  
  estimates <- mutate(estimates, lower = Estimate - 1.96 * `Std. Error`,
                      upper = Estimate + 1.96 * `Std. Error`) %>%
    mutate_all(round, 2)
  
  result <- mutate(estimates, result = paste0( Estimate, ' (', lower, ' - ', upper, ')')) %>%
    select(result)
  
  return(result)
}


female_results <- get_slope_intercept(df.risk.f)

names(female_results) <- paste0(names(female_results), ' female')

male_results <- get_slope_intercept(df.risk.m)

names(male_results) <- paste0(names(male_results), ' male')



cal_slope_intercept <- cbind( female_results, male_results) %>%
  select(1,3,2,4)

rownames(cal_slope_intercept) <- c('Calibration intercept', 'Calibration slope')

write_csv(cal_slope_intercept,  paste0('./cal_slope_intercept_', z.period, '.csv'))


