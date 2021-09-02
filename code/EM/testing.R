
z.xlab <- "Date"
g1 <-ggsurvplot(z, data = df, fun="event", risk.table = FALSE, legend.labs=levels(as.factor(df[,z.var])),
                legend.title ="Age groups", surv.scale="percent",
                censor=FALSE) +
  labs(x=z.xlab, y="percentage Vaccinated (dose 1)")
#g1$plot <- g1$plot +ggplot2::annotate("text",x=15,y=z.ymax, label = paste0("HR = ", z.t[1], "\n(",z.t[2],", ",z.t[3],")" ))


print(g1)

png("/conf/EAVE/GPanalysis/outputs/progs/EM/vaccine_by_age.png")
g1$plot + 
  scale_x_continuous(breaks = c(0,10,20,30,40,50, 60),
                     labels = c("08-Dec", "18-Dec", "28-Dec", "7-Jan", "17-Jan", "27-Jan", "6-Feb" ))
dev.off()
