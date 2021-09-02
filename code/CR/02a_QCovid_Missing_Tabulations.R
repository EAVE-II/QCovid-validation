#02a_QCovid_Missing_Tabulations.R
#Calculate the impute values for bmi


#description of the missingness on BMI and ethnicity

vars <- names(df)[c(87, 72, 75, 88, 89, 22:49)]
#vars <- names(df)[c(87, 72, 75, 88)]

z <- df %>% mutate(Missing = if_else(is.na(BMI), "Yes","No")) %>% 
  group_by(Missing) %>% dplyr::summarise_at(vars, mean) %>% 
  pivot_longer(cols=all_of(vars)) %>% 
  arrange(name, Missing) %>% 
  relocate(name, .before=Missing) %>% 
  dplyr::rename(Condition = name, Prop_with_Condition=value)

z <- df %>% mutate(Miss_bmi = if_else(is.na(BMI), 1,0),
                   Miss_eth = if_else(new_ethnic_grp=="Not recorded", 1,0)) %>% 
  dplyr::select_at(all_of(c(vars, "Miss_bmi", "Miss_eth"))) %>% 
  mutate_at(vars, as.factor) %>% 
  pivot_longer(cols=all_of(vars)) %>% 
  group_by(name, value) %>% dplyr::summarise(prop_miss_bmi = mean(Miss_bmi),prop_miss_eth = mean(Miss_eth) ) 
 