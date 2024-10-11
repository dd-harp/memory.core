## ------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## ----eval=F--------------------------------------------------------------------------------------
## purl("Figure7.Rmd", "Figure7.R")


## ------------------------------------------------------------------------------------------------
foiP3 = list(hbar = 1, 
             agePar = par_type2Age(), 
             seasonPar = par_sinSeason(), 
             trendPar = par_flatTrend())


## ------------------------------------------------------------------------------------------------
Psimple = function(h=10/365,  FoIpar=foiP3, r=1/200, tau=0, 
                   Tmax=5*365, alphaMax = 420, dt=5){
  a = seq(40, Tmax, by=10) 
  Bmesh = seq(.1, 12.9, by = 0.1)
  mesh = outer(Bmesh, a)
  ages = mesh*0
  for(j in 1:length(a)){
    ages[,j] = d_clone_density(Bmesh,a[j], FoIpar=FoIpar, hhat=h, r=r, tau=tau)
  }
  
  list(pd = t(ages), x=a, y=Bmesh)
}


## ------------------------------------------------------------------------------------------------
Psimple() -> simpleP


## ------------------------------------------------------------------------------------------------
Figure7a = function(simpleP){
  nclrs = 50 
  clrs = rev(magma(50))

  with(simpleP, filled.contour(x, y, pd, 
                             xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                             ylab = expression(list(xi, paste("Parasite Densities"))), 
                             xaxt = "n", 
                             yaxt = "n", 
                             nlevels=nclrs, 
                             col = clrs)) 

  mtext("a) Parasite Density Distributions in Simple Infections", 3, 1, at=350)
}
Figure7a(simpleP)


## ------------------------------------------------------------------------------------------------
png("Figure7a.png", height= 360, width = 720)
Figure7a(simpleP)
invisible(dev.off(dev.cur()))


## ------------------------------------------------------------------------------------------------
Pcomplex = function(h=10/365,  FoIpar=foiP3, r=1/200, tau=0, 
                    Tmax=5*365, alphaMax = 420, dt=5){
  a = seq(40, Tmax, by=10) 
  Bmesh = seq(.1, 12.9, by = 0.1)
  mesh = outer(Bmesh, a)
  ages = mesh*0
  for(j in 1:length(a)){
    ages[,j] = d_parasite_density(Bmesh,a[j], FoIpar=FoIpar, hhat=h, r=r, tau=tau)
  }
  
  list(pd = t(ages), x=a, y=Bmesh)
}


## ------------------------------------------------------------------------------------------------
Pcomplex() -> complexP


## ------------------------------------------------------------------------------------------------
Figure7b = function(complexP){
nclrs = 50 
clrs = rev(magma(50))

with(complexP, filled.contour(x, y, pd, 
                              xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                              ylab = expression(list(xi, paste("Parasite Densities"))), 
                              xaxt = "n", 
                              yaxt = "n", 
                              nlevels=nclrs, 
                              col = clrs)) 

mtext("b) Parasite Density Distributions in Complex Infections", 3, 1, at=365)
}
Figure7b(complexP)


## ------------------------------------------------------------------------------------------------
png("Figure7b.png", height= 360, width = 720)
Figure7b(complexP)
invisible(dev.off(dev.cur()))


## ------------------------------------------------------------------------------------------------
CountsSurface = function(dBobj, par=par_nb()){
  surf=dBobj$pd
  a = dBobj$x 
  meshX = dBobj$y 
  bins = c(log10(1:9), seq(1, 5, length.out=30)) 
  counts= outer(a, bins)*0
  for(i in 1:length(a)){
    for(j in 1:length(meshX)){
      Bx = surf[i,j]
      cnts = d_nz_counts_log_binned(meshX[j], bins, 13, par)
      counts[i,]= counts[i,] + Bx*cnts
    } 
  }
  list(counts=counts, x=a, y=bins) 
}


## ------------------------------------------------------------------------------------------------
countsSurf = CountsSurface(complexP)


## ------------------------------------------------------------------------------------------------
Figure7c = function(countsSurf){
  nclrs = 41 
  clrs = rev(magma(nclrs))

  with(countsSurf, filled.contour(x, y, counts, 
                                xlab = expression(list(a, "Host Cohort Age (in Days)")), 
                                ylab = expression(list(hat(xi), log[10](paste("Parasite Counts")))), 
                                xaxt = "n", 
                                yaxt = "n", 
                                nlevels=nclrs, 
                                col = clrs)) 

  mtext("c) Parasite counts", 3, 1, at=100)
}
Figure7c(countsSurf)


## ------------------------------------------------------------------------------------------------
png("Figure7c.png", height= 360, width = 720)
Figure7c(countsSurf)
invisible(dev.off(dev.cur()))

