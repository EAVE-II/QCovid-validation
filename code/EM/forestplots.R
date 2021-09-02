###brrom tidy doesnt work on my model - but i can manually create that tablt pretty easily.
library(forestplot)
library(broom)

z.fit.M.H <- CR_Model_Hosp_Male

##FEMALES HOSP######
STAT <- summary(z.fit.F.H)$coef
CI <- summary(z.fit.F.H)$conf.int
#"term"      "estimate"  "std.error" "statistic" "p.value"   "conf.low"  "conf.high"
statend <- dim(STAT)[1]
ciend <- dim(CI)[1]


term <- rownames(STAT)[7:statend]
estimate <- as.numeric(CI[37:ciend,1])
conf.low<- as.numeric(CI[37:ciend,3])
conf.high<- as.numeric(CI[37:ciend,4])
std.error <- as.numeric(STAT[7:statend,2])
statistic <- as.numeric(STAT[7:statend,4])
p.value <-as.numeric(STAT[7:statend,6])
mod.df <- as.data.frame(cbind(term, estimate,std.error ,statistic, p.value ,conf.low, conf.high))


vars <- c(  "estimate",  "std.error" ,"statistic", "p.value" ,  "conf.low" , "conf.high")
mod.df <-mod.df %>% mutate_at(vars, as.character) %>% mutate_at(vars, as.numeric)
mod.df$term <- as.character(mod.df$term)
saveRDS(mod.df, "/conf/EAVE/GPanalysis/outputs/temp/QCovidcoeff_female_hosp_exclsplines.rds")
# pick up the first 4 values 
df1 <- mod.df %>% 
  transmute( 
    HR = estimate, 
    low = conf.low, 
    high = conf.high
    )
#df1 <- df1 %>% mutate(HR = ifelse(HR >50 , "", HR))%>% 
#  mutate( low = ifelse(HR =="" , "", low), 
#                      high = ifelse(HR  =="" , "", high))
row_names <- cbind(c("Name", mod.df$term),
                   c("HR", round(df1$HR,2)))
df1 <- rbind(rep(NA, 4), df1)

png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/female_hosp_1.png")
forestplot(labeltext = row_names[c(1:6, 35:42),],
           df1[c(1:6, 35:42), c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=1)), 
           title = "Females - hospitalisation (part1)")
dev.off()


png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/female_hosp_2.png")
forestplot(labeltext = row_names[c(1, 7:34, 43:49),],
           df1[c(1, 7:34, 43:49),c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.8)), 
           title = "Females - hospitalisation (part2)")
dev.off()


##MALES HOSP######
rows_modH[!grepl("ps", rows_modH)]

niceplot <- function(df, cex=1, title = "plot of hazard ratios",  cutpoint = 0){

STAT <- as.data.frame(summary(z.fit.M.H)$coef)
STAT <- STAT[!grepl("pspline", rownames(STAT)),]
CI <- as.data.frame(summary(z.fit.M.H)$conf.int)
CI <- CI[!grepl("ps\\(", rownames(CI)),]  
#"term"      "estimate"  "std.error" "statistic" "p.value"   "conf.low"  "conf.high"
term <- rownames(STAT)
estimate <- as.numeric(CI[,1])
conf.low<- as.numeric(CI[,3])
conf.high<- as.numeric(CI[,4])
std.error <- as.numeric(STAT[,2])
statistic <- as.numeric(STAT[,4])
p.value <-as.numeric(STAT[,6])
mod.df <- as.data.frame(cbind(term, estimate,std.error ,statistic, p.value ,conf.low, conf.high))

length(estimate)
vars <- c(  "estimate",  "std.error" ,"statistic", "p.value" ,  "conf.low" , "conf.high")
mod.df <-mod.df %>% mutate_at(vars, as.character) %>% mutate_at(vars, as.numeric)
mod.df$term <- as.character(mod.df$term)

#saveRDS(mod.df, "/conf/EAVE/GPanalysis/outputs/temp/QCovidcoeff_male_hosp_exclsplines.rds")
# pick up the first 4 values 

#mod.df <- readRDS("/conf/EAVE/GPanalysis/outputs/temp/QCovidcoeff_male_hosp_exclsplines.rds")
df1 <- mod.df %>% 
  transmute( 
    HR = estimate, 
    low = conf.low, 
    high = conf.high
  )
#df1 <- df1 %>% mutate(HR = ifelse(HR >50 , "", HR))%>% 
#  mutate( low = ifelse(HR =="" , "", low), 
#                      high = ifelse(HR  =="" , "", high))


row_names <- cbind(c("Name", mod.df$term),
                   c("HR", round(df1$HR,2)))
df1 <- rbind(rep(NA, 4), df1)


png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/male_hosp_1.png")
forestplot(labeltext = row_names , #[c(1:6, 35:42),],
           df1[, c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.6)), 
           title = "Males - hospitalisation (part1). Adjusted for age, BMI")
dev.off()


png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/male_hosp_2.png")
forestplot(labeltext = row_names[c(1, 7:34, 43:49),],
           df1[c(1, 7:34, 43:49),c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.8)), 
           title = "Males - hospitalisation (part2) Adjusted for age, BMI")
dev.off()

###female deaths#############
STAT <- summary(z.fit.F.D)$coef
CI <- summary(z.fit.F.D)$conf.int
#"term"      "estimate"  "std.error" "statistic" "p.value"   "conf.low"  "conf.high"
statend <- dim(STAT)[1]
ciend <- dim(CI)[1]


term <- rownames(STAT)[7:statend]
estimate <- as.numeric(CI[37:ciend,1])
conf.low<- as.numeric(CI[37:ciend,3])
conf.high<- as.numeric(CI[37:ciend,4])
std.error <- as.numeric(STAT[7:statend,2])
statistic <- as.numeric(STAT[7:statend,4])
p.value <-as.numeric(STAT[7:statend,6])
mod.df <- as.data.frame(cbind(term, estimate,std.error ,statistic, p.value ,conf.low, conf.high))


vars <- c(  "estimate",  "std.error" ,"statistic", "p.value" ,  "conf.low" , "conf.high")
mod.df <-mod.df %>% mutate_at(vars, as.character) %>% mutate_at(vars, as.numeric)
mod.df$term <- as.character(mod.df$term)
saveRDS(mod.df, "/conf/EAVE/GPanalysis/outputs/temp/QCovidcoeff_female_death_exclsplines.rds")
# pick up the first 4 values 
df1 <- mod.df %>% 
  transmute( 
    HR = estimate, 
    low = conf.low, 
    high = conf.high
  )
#df1 <- df1 %>% mutate(HR = ifelse(HR >50 , "", HR))%>% 
#  mutate( low = ifelse(HR =="" , "", low), 
#                      high = ifelse(HR  =="" , "", high))
row_names <- cbind(c("Name", mod.df$term),
                   c("HR", round(df1$HR,2)))
df1 <- rbind(rep(NA, 4), df1)

png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/female_death_1.png")
forestplot(labeltext = row_names[c(1:6, 35:39),],
           df1[c(1:6, 35:39), c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=1)), 
           title = "Females - deaths (part1), adjusted for age, BMI (splines)")
dev.off()


png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/female_death_2.png")
forestplot(labeltext = row_names[c(1, 7:34, 43:46),],
           df1[c(1, 7:34, 43:46),c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.8)), 
           title = "Females - deaths (part2), adjusted for age, BMI (splines)")
dev.off()


##male deaths#######
STAT <- summary(z.fit.M.D)$coef
CI <- summary(z.fit.M.D)$conf.int
#"term"      "estimate"  "std.error" "statistic" "p.value"   "conf.low"  "conf.high"
statend <- dim(STAT)[1]
ciend <- dim(CI)[1]


term <- rownames(STAT)[7:statend]
estimate <- as.numeric(CI[37:ciend,1])
conf.low<- as.numeric(CI[37:ciend,3])
conf.high<- as.numeric(CI[37:ciend,4])
std.error <- as.numeric(STAT[7:statend,2])
statistic <- as.numeric(STAT[7:statend,4])
p.value <-as.numeric(STAT[7:statend,6])
mod.df <- as.data.frame(cbind(term, estimate,std.error ,statistic, p.value ,conf.low, conf.high))


vars <- c(  "estimate",  "std.error" ,"statistic", "p.value" ,  "conf.low" , "conf.high")
mod.df <-mod.df %>% mutate_at(vars, as.character) %>% mutate_at(vars, as.numeric)
mod.df$term <- as.character(mod.df$term)
saveRDS(mod.df, "/conf/EAVE/GPanalysis/outputs/temp/QCovidcoeff_male_death_exclsplines.rds")
# pick up the first 4 values 
df1 <- mod.df %>% 
  transmute( 
    HR = estimate, 
    low = conf.low, 
    high = conf.high
  )
#df1 <- df1 %>% mutate(HR = ifelse(HR >50 , "", HR))%>% 
#  mutate( low = ifelse(HR =="" , "", low), 
#                      high = ifelse(HR  =="" , "", high))
row_names <- cbind(c("Name", mod.df$term),
                   c("HR", round(df1$HR,2)))
df1 <- rbind(rep(NA, 4), df1)

png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/male_death_1.png")
forestplot(labeltext = row_names[c(1:6, 35:40),],
           df1[c(1:6, 35:40), c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=1)), 
           title = "Males - deaths (part1), adjusted for age, BMI (splines)")
dev.off()


png("/conf/EAVE/GPanalysis/outputs/EM/QCovid/male_death_2.png")
forestplot(labeltext = row_names[c(1, 7:34, 41:47),],
           df1[c(1, 7:34, 41:47),c("HR", "low", "high")],
           is.summary=c(FALSE, FALSE, FALSE),
           zero      = 1,
           clip=c(0.1,5), 
           xlog      = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "", cex=.8)), 
           title = "Males - deaths (part2), adjusted for age, BMI (splines)")
dev.off()