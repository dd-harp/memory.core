## ----------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## ----------------------------------------------------------------------------------------------------
clrs = rev(turbo(50))

par(mar = c(5,5,3,5))

foiP3 = list(hbar = 5/365, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())

base_pars = par_Fmu_base()


## ----------------------------------------------------------------------------------------------------
clrs = rev(turbo(100))
thou = 0:1000
daoi <- dAoI(thou, 5*365, foiP3) 
daoy <- dAoY(thou, 5*365, foiP3) 

mu_alpha =  Fmu(thou, 0, base_pars)

scl =  max(mu_alpha)/max(daoy)


## ----------------------------------------------------------------------------------------------------
plot(thou, mu_alpha, type = "l", ylim = c(0,11),
     xlab = expression(list(alpha, paste("Parasite Age (in Days)"))), 
     ylab = expression(list(F[mu](alpha), paste("Parasite Densities"))))

lines(thou, scl*daoi, lwd=2)
lines(thou, scl*daoy, col = grey(0.8), lwd=2)

i = 1:100
points(10*i, scl*daoi[10*i], col=clrs, pch = 15, cex=0.4) 
points(10*i, mu_alpha[10*i], col=clrs, pch = 15, cex=0.4) 
tks = pretty(daoy)
axis(4, tks*scl, tks)
mtext(expression(list(f[A](alpha), f[Y](alpha))), 4,3)

mtext("a) Expected Logged Parasite Densities, Density of Infections by Age", 3, 1, at = 360, cex=1)



## ----------------------------------------------------------------------------------------------------
parD = par_Omega_beta()

clrs = rev(turbo(100))
dt = 0.1
xx = seq(0, 13, by=dt)
Pa = d_clone_density(xx, 5*365, foiP3)*dt


## ----------------------------------------------------------------------------------------------------
plot(xx, Pa, type = "l",
     ylab = expression(f[P](xi)),
     xlab = expression(list(xi, paste("Parasite Densities"))))


segments(6,0, 6, 0.004, lty=2)
  
mu = Fmu(20, 0, base_pars)
fA = dAoI(20, 5*365, foiP3)
dd = d_Omega(xx, mu, 13, parD)
scl = max(Pa)/max(dd*fA)

for(i in (1:100)){
  alpha = 1010-10*i
  mu = Fmu(alpha, 0, base_pars)
  fA = dAoI(alpha, 5*365, foiP3)
  dd = d_Omega(xx, mu, 13, parD)
  lines(xx, fA*dd*scl, col = clrs[101-i])
}

tks = pretty(Pa/40, 6)

axis(4, tks*40, tks)
mtext(expression(f[A](alpha)*Omega(xi,F[mu](alpha))), 4, 3)
segments(6,0, 6, 0.17, col = grey(0.5), lty=2)

mtext("b) Parasite Density Distributions, Simple Infections", 3, 1, at = 3.3, cex=1)



## ----------------------------------------------------------------------------------------------------
cdfPa = cumsum(Pa)
cdfPa2 = cdfConvolve2(xx, cdfPa, cdfPa)

cdfPa_by_moi = list() 
cdfPa_by_moi[[1]] = cdfPa
cdfPa_by_moi[[2]] = cdfPa2

cdfPaN = cdfPa2 
for(N in 3:10){
  cdfPaN =  cdfConvolve2(xx, cdfPa, cdfPaN) 
  cdfPa_by_moi[[N]] = cdfPaN 
}

Ba = d_parasite_density(xx, 5*365, foiP3, 5/365)

clrs = viridis(12)

xxn1 = xx[-1] - dt/2


## ----------------------------------------------------------------------------------------------------
plot(xxn1, diff(cdfPa_by_moi[[10]]), type = "l", col = clrs[10],
     ylab = expression(list(f[P](xi), f[N](xi),f[B](xi), f[bar(B)](xi))),
     xlab = expression(list(xi, paste("Parasite Densities"))))

for(N in 9:1)
  lines(xxn1, diff(cdfPa_by_moi[[N]]), col = clrs[N])

lines(xx, Ba, col = "darkorange", lwd=3)
lines(xx, Ba)

mtext("c) Parasite Density Distributions, Complex Infections", 3, 1, at = 3.5, cex=1)


