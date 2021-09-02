df.risk.f <- readRDS("df_risk_f.RDS")

z <- df.risk.f[1:10000,]
z <- df.risk.f

z.res <- D.index(x = z$Qrisk_hosp, 
        surv.time = z$Time.To.Hosp,
        surv.event = z$hosp_covid)

> library(iterativeBMA)
Loading required package: BMA
Loading required package: leaps
Loading required package: robustbase

Attaching package: 'robustbase'

The following object is masked from 'package:survival':
  
  heart

Loading required package: inline

Attaching package: 'inline'

The following object is masked from 'package:htmltools':
  
  code

Loading required package: rrcov
Scalable Robust Estimators with High Breakdown Point (version 1.5-5)


Attaching package: 'rrcov'

The following object is masked from 'package:nlme':
  
  getData

Loading required package: Biobase
Loading required package: BiocGenerics
Loading required package: parallel

Attaching package: 'BiocGenerics'

The following objects are masked from 'package:parallel':
  
  clusterApply, clusterApplyLB, clusterCall, clusterEvalQ, clusterExport, clusterMap, parApply,
parCapply, parLapply, parLapplyLB, parRapply, parSapply, parSapplyLB

The following objects are masked from 'package:lubridate':
  
  intersect, setdiff, union

The following objects are masked from 'package:dplyr':
  
  combine, intersect, setdiff, union

The following objects are masked from 'package:stats':
  
  IQR, mad, sd, var, xtabs

The following objects are masked from 'package:base':
  
  anyDuplicated, append, as.data.frame, basename, cbind, colnames, dirname, do.call, duplicated, eval,
evalq, Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply, Map, mapply, match, mget, order,
paste, pmax, pmax.int, pmin, pmin.int, Position, rank, rbind, Reduce, rownames, sapply, setdiff, sort,
table, tapply, union, unique, unsplit, which.max, which.min

Welcome to Bioconductor

Vignettes contain introductory material; view with 'browseVignettes()'. To cite Bioconductor, see
'citation("Biobase")', and for packages 'citation("pkgname")'.


Attaching package: 'Biobase'

The following object is masked from 'package:robustbase':
  
  rowMedians

> brier.score
function (predictedArr, truthArr) 
{
  if (length(predictedArr) != length(truthArr)) {
    print("ERROR: length NOT equal!!")
  }
  temp.vec <- (truthArr - predictedArr)^2
  sum(temp.vec)
}
<bytecode: 0x000000005f3fdb88>
  <environment: namespace:iterativeBMA>
  > z <- df.risk.f
> brier.score(z$Qrisk_death/100, z$covid_cod)
[1] 696.1096
> brier.score(z$Qrisk_hosp/100, z$covid_hosp)
[1] "ERROR: length NOT equal!!"
[1] 0
> brier.score(z$Qrisk_hosp/100, z$hosp_covid)
[1] 2750.222
> brier.score(z$Qrisk_death/100, z$covid_cod)
[1] 696.1096
> brier.score(z$Qrisk_death, z$covid_cod)
[1] 141734.4
> brier.score(z$Qrisk_death/1000, z$covid_cod)
[1] 699.2244
> brier.score(z$Qrisk_death/100, z$covid_cod)
[1] 696.1096
> brier.score(z$Qrisk_death/100/max(z$Qrisk_death), z$covid_cod)
[1] 700.523
> summary(z$Qrisk_death)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.00013  0.00067  0.00312  0.04481  0.01986 39.46469 
> summary(z$Qrisk_death/max(z$Qrisk_death))
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000032 0.0000171 0.0000791 0.0011353 0.0005032 1.0000000 
> brier.score(z$Qrisk_death/max(z$Qrisk_death), z$covid_cod)
[1] 744.1706
> brier.score(z$Qrisk_death/max(z$Qrisk_death), z$covid_cod)/nrow(z)
[1] 0.0003298222
> brier_score(z$Qrisk_death/max(z$Qrisk_death), z$covid_cod)
Error in brier_score(z$Qrisk_death/max(z$Qrisk_death), z$covid_cod) : 
  could not find function "brier_score"
> getwd()
[1] "\\\\isdsf00d03/EAVE/GPanalysis/QCovid_Validation/progs/CR"