## -------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## -------------------------------------------------------------------------------------------------
foiP3 = list(hbar = 1, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())


## -------------------------------------------------------------------------------------------------
aa = seq(5, 5*365, by = 5)


## ----eval=F---------------------------------------------------------------------------------------
## meanP = sapply(aa, moments_clone_density, FoIpar=foiP3, hhat=5/365)
## meanB = sapply(aa, moments_parasite_density, FoIpar=foiP3, hhat=5/365)
## write.table(data.frame(P=meanP, B=meanB), "PB.txt")


## -------------------------------------------------------------------------------------------------
PB = read.table("PB.txt")
meanP = PB$P
meanB = PB$B


## -------------------------------------------------------------------------------------------------
pFmu = par_Fmu_base()
solve_dAoYda(5/365, foiP3, Amax=5*365, dt=5) -> hybrid
tm = hybrid$time
approxP = Fmu(hybrid$x, 0, pFmu)
approxB = Fmu(hybrid$y, 0, pFmu)

plot(aa, meanB, type = "l", ylim = c(0, 13), col = "darkred", 
     xlab = "a - Cohort Age", 
     ylab = expression(list(xi, paste(log[10], " Parasite Densities"))))
#lines(tm, approxB, col = "salmon3", lwd=2)
lines(aa, meanP, type = "l", ylim = c(0, 13), col = "darkblue")
lines(tm, approxP, col = "cyan4")
mtext("Expected Densities Exactly vs. Hybrid Model Predictions", 3, 1, at=365)


## ----purl Figure 8, eval=F------------------------------------------------------------------------
## print("Making Figure 8")
## purl("Figure8.Rmd", "Figure8.R")


## ----source Figure 8, eval=F----------------------------------------------------------------------
## png("Figure8.png", height=360, width=540)
## source("Figure8.R")
## invisible(dev.off(dev.cur()))

