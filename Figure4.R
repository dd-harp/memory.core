## -------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## ----eval=F---------------------------------------------------------------------------------------
## purl("Figure4.Rmd", "Figure4.R")


## -------------------------------------------------------------------------------------------------
foiP3 = list(hbar = 1, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())


## -------------------------------------------------------------------------------------------------
nclrs = 25
clrs = rev(magma(nclrs))

MoIsurface = function(h = 10/365, FoIpar=foiP3, r=1/200, tau=0, 
                      Tmax = 5*365, alphaMax = 500, dt=5)
{ 
  zeta = seq(0, 12, by=1) 
  a = seq(90, Tmax, by=dt) 
  mesh = outer(zeta, a)
  moi = mesh*0
  
  for(j in 1:length(a)){
    m = meanMoI(a[j], FoIpar, tau, h, r)
    moi[,j] = dpois(zeta, m) 
  } 
  
  list(moi=t(moi), x=a, y=zeta)
}

moiObj = MoIsurface()


with(moiObj, filled.contour(x, y, moi, 
                            xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                            ylab = expression(f[M](zeta, a)), 
                            xaxt = "n", 
                            yaxt = "n", 
                            nlevels=nclrs, 
                            col = clrs)) 


mtext("a) Multiplicity of Infection (MoI)", 3, 1, at = 220, cex=1)


## -------------------------------------------------------------------------------------------------
nclrs = 29 
clrs = rev(magma(nclrs))

AoIsurface = function(h=10/365, FoIpar=foiP3, r=1/200, tau=0, 
                      Tmax=5*365, alphaMax = 420, dt=5){ 
  alpha = seq(2, alphaMax, by=dt) 
  a = seq(90,Tmax, by=dt) 
  mesh = outer(alpha, a)
  ages = mesh*0
  
  for(j in 1:length(a)){
    ix = which(alpha <= a[j])
    ages[ix,j] = dAoI(alpha[ix], a[j], FoIpar=FoIpar,hhat=h,r=r,tau=tau)
  } 

  list(aoi=t(ages), x=a, y=alpha)
}

aoiObj = AoIsurface() 

with(aoiObj, filled.contour(x, y, aoi, 
                            xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                            ylab = expression(f[A](alpha, a)), 
                            xaxt = "n", 
                            yaxt = "n", 
                            nlevels=nclrs, 
                            col = clrs)) 


mtext("b) Age of Infection (AoI)", 3, 1, at=172, cex=1)


## -------------------------------------------------------------------------------------------------
nclrs = 29 
clrs = rev(magma(nclrs))

AoYsurface = function(h=10/365, FoIpar=foiP3, r=1/200, tau=0, 
                      Tmax=5*365, alphaMax = 420, dt=5){ 
  alpha = seq(2, alphaMax, by=dt) 
  a = seq(90,Tmax, by=dt) 
  mesh = outer(alpha, a)
  ages = mesh*0
  
  for(j in 1:length(a)){
    ix = which(alpha <= a[j])
    ages[ix,j] = dAoY(alpha[ix], a[j], FoIpar=FoIpar,hhat=h,r=r,tau=tau)
  } 
  
  list(aoy=t(ages), x=a, y=alpha)
}

aoyObj = AoYsurface() 

with(aoyObj, filled.contour(x, y, aoy, 
                            xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                            ylab = expression(f[Y](alpha, a)), 
                            xaxt = "n", 
                            yaxt = "n", 
                            nlevels=nclrs, 
                            col = clrs))

mtext("c) Age of the Youngest Infection (AoY)", 3, 1, at=280, cex=1)

