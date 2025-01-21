## -----------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)


## -----------------------------------------------------------------------------
clrs = plasma(8)


par_t2 = par_type2Age() 

a20yearsBy10days=seq(0, 7300, by=10)
wt1 = ageFoI(a20yearsBy10days, par_type2Age())
wt2 = ageFoI(a20yearsBy10days, par_flatAge())


## -----------------------------------------------------------------------------
plot(a20yearsBy10days/365, wt1, type = "l", xlab = "Age in Years", ylab = expression(omega(a)), col = clrs[1])
lines(a20yearsBy10days/365, wt2, col = clrs[5])


## -----------------------------------------------------------------------------
p1 = par_expSeason(phase=-90, pwr=3.5, lift=0) 
p2 = par_sinSeason(lift=-0.85, pwr=4, phase=0)
p3 = par_sinSeason(lift=0, pwr=4, phase=-90)
p4 = par_sinSeason(phase=-90)
p5 = par_flatSeason()


## -----------------------------------------------------------------------------
oneYear = c(0:365)
plot(oneYear,  seasonalFoI(oneYear, p1), type = "l", ylab = expression(S(t) - paste("Seasonal Pattern")), xlab = "Day of Year", col = clrs[1]) 
lines(oneYear, seasonalFoI(oneYear, p2), col = clrs[3], lwd=1.5) 
lines(oneYear, seasonalFoI(oneYear, p3), col = clrs[5], lwd=1.5) 
lines(oneYear, seasonalFoI(oneYear, p4), col = clrs[6], lwd=2) 
lines(oneYear, seasonalFoI(oneYear, p5), col = grey(0.5), lwd=1.5)

