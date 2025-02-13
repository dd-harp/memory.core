## --------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)
library(knitr)


## --------------------------------------------------------------------------------
par_F1 = par_Fmu_base(tildeb = fits1[1], Sa=fits1[2])
par_F2 = par_Fmu_base(tildeb = fits2[1], Sa=fits2[2])
par_F3 = par_Fmu_base(tildeb = fits3[1], Sa=fits3[2])
par_F4 = par_Fmu_base(tildeb = fits4[1], Sa=fits4[2])
par_F5 = par_Fmu_base(tildeb = fits5[1], Sa=fits5[2])


## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
pdObj = alpha2PD()


## --------------------------------------------------------------------------------
nclrs = 29 
clrs = rev(magma(nclrs))

with(pdObj, filled.contour(x, y, pd, 
                           xlab = expression(list(alpha, "Parasite Age (in Days)")), 
                           ylab = expression(f[P](xi, alpha)), 
                           xaxt = "n", 
                           yaxt = "n", 
                           nlevels=nclrs, 
                           col = clrs))


