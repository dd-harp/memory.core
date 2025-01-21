## ----------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## ----------------------------------------------------------------------------------------------------
alpha2PD = function(r=1/200, tau=0, Tmax=5*365, dt=1){
  alpha = seq(8, 400, by=2) 
  Bmesh = seq(0.1, 12.9, by=0.1)
  mesh = outer(Bmesh, alpha)*0
  for(j in 1:length(alpha)){
    pd = d_alpha2density(Bmesh, alpha[j], r, parD) 
    mesh[,j] = pd
  }
  mesh 
  list(pd=t(mesh), x=alpha, y=Bmesh)
}


## ----------------------------------------------------------------------------------------------------
pdObj = alpha2PD()


## ----------------------------------------------------------------------------------------------------
nclrs = 29 
clrs = rev(magma(nclrs))

with(pdObj, filled.contour(x, y, pd, 
                           xlab = expression(list(alpha, "Parasite Age (in Days)")), 
                           ylab = expression(f[P](xi, alpha)), 
                           xaxt = "n", 
                           yaxt = "n", 
                           nlevels=nclrs, 
                           col = clrs))


