## ------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## ------------------------------------------------------------------------------------------------
foiP3 = list(hbar = 1, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())


## ------------------------------------------------------------------------------------------------
foiP3 = list(hbar = 5/365, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())


## ------------------------------------------------------------------------------------------------
aa = seq(5, 5*365, by = 5)


## ----eval=F--------------------------------------------------------------------------------------
## pr = sapply(aa, d_parasite_detect, FoIpar=foiP3)
## PR = data.frame(pr=as.vector(pr))
## write.table(PR, "pr.txt")


## ------------------------------------------------------------------------------------------------
moi = meanMoI(aa, foiP3)

pr = read.table("pr.txt")$pr


## ------------------------------------------------------------------------------------------------
solve_dAoYda(1, foiP3, Amax=5*365, dt=5) -> hybrid
tm = hybrid$time
x = hybrid$x[-1]
y = hybrid$y[-1]
m = hybrid$m[-1]
ix = 1:length(x)
FQx =  ix*0
FQy =  ix*0

for(i in ix){
  FQx[i] = FQ(x[i], aa[i], foiP3)
  FQy[i] = FQ(y[i], aa[i], foiP3)
}

xct = pr*(1-exp(-moi)) 
aprx = 1 - exp(-m*FQx)
#aprx1 = (1-exp(-m))*FQy


## ------------------------------------------------------------------------------------------------
plot(aa, xct, type = "l", ylim = c(0,1),
     xlab = "a - Cohort Age",
     ylab = expression(hat(p)[tau](a)))
lines(aa, aprx, col = "salmon")

#lines(aa, (aprx + aprx1)/2, col = "darkgreen")
#lines(aa, aprx1, col = "blue")


#plot(aa, xct-aprx, type = "l")
#segments(0,0, max(aa), 0)





## ----purl Figure 10, eval=F----------------------------------------------------------------------
## print("Making Figure 10")
## purl("Figure10.Rmd", "Figure10.R")


## ----source Figure 10, eval=F--------------------------------------------------------------------
## png("Figure10.png", height=360, width=540)
## source("Figure10.R")
## invisible(dev.off(dev.cur()))

