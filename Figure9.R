## ------------------------------------------------------------------------------------------------
library(ramp.falciparum)
library(viridisLite)


## ------------------------------------------------------------------------------------------------
clrs8d = viridis(8)


## ------------------------------------------------------------------------------------------------

Figure9a = function(xl=0, xh=13){
  
  xl = 0  
  xh = 13  
  meshX=seq(xl,xh,by=0.1)
  
  DD <- d_detect(meshX, bvm=13, pars=par_nb(s=6))
  CB <- t(sapply(meshX, d_nz_counts_log_binned, bins=c(1:5, 13), pars=par_nb(s=6)))
  
  plot(meshX, CB[,1], type = "n", xlab =expression(log[10](B)), ylab = "Proportion", ylim = c(0,1), col = "white", xlim = c(xl, xh), xaxt = "n")
  
  cbi = 1-DD 
  polygon(c(xl, meshX,13), 1-c(0, cbi,0), col = clrs8d[8], border=clrs8d[8]) 
  
  cb0 = cbi
  cbi = CB[,1]
  polygon(c(meshX, rev(meshX)), 1-c(cbi, rev(cb0)), col = clrs8d[7], border=clrs8d[7]) 
  
  for(i in 2:6){
    cb0 = cbi
    cbi = rowSums(CB[,1:i])
    polygon(c(meshX, rev(meshX)), 1-c(cbi, rev(cb0)), col = clrs8d[8-i], border=clrs8d[8-i]) 
  }  
  
  #cbi = 1-DD 
  #polygon(c(xl, meshX), 1-c(0, cbi), col = "red", border=clrs8d[8]) 
  
  ssrt=62
  ccx=1.2
  text(4.5, 0.8, "Parasites not Detected", cex=ccx)
  
  text(7.3, .25, expression(1-10), col = "white", cex=ccx, srt = ssrt)
  text(8.4, .25, expression(10-10^2), col = "white", cex=ccx, srt = ssrt)
  text(9.4, .25, expression(10^2-10^3), col = "white", cex=ccx, srt = ssrt)
  text(10.4, 0.25, expression(10^3-10^4), col = "white", cex=ccx, srt = ssrt)
  text(11.4, .25, expression(10^4-10^5), col = "white", cex=ccx, srt = ssrt)
  text(12.1, 0.2, expression(10^5-infinity), col = "white", cex=ccx, srt=ssrt)
  axis(1, 1:12, 1:12)
  mtext("a) Probability of Detection", 3, 1, at=1)
}
Figure9a()

## ------------------------------------------------------------------------------------------------
png("Figure9a.png", height= 360, width = 720)
Figure9a()
invisible(dev.off(dev.cur()))


## ------------------------------------------------------------------------------------------------
Figure9b = function(){
  pp = par_nb(s=7)
  b2c = d_nz_counts_log_binned(1, bins = c(1:5, 13), pars=pp)
  b2d = 1-d_detect(1,pars=pp)
  b2cl = c(b2d, b2c*(1-b2d))
  plot(3-0.05+c(-3:3)/10, b2cl, type = "h", lwd=5, col = rev(clrs8d)[-2], xlim = c(2.5,11.5), xaxt = "n", xlab = expression(log[10](B)), ylab="Proportion") 
  for(i in c(4:11)){
    b2c = d_nz_counts_log_binned(i,bins = c(1:5, 13), pars=pp)
    b2d = 1-d_detect(i,pars=pp)
    b2cl = c(b2d, b2c*(1-b2d)) 
    lines(i -0.05 + c(-3:3)/10, b2cl, type = "h", lwd=5, col = rev(clrs8d)[-2])
    axis(1, 3:11, 3:11)
  }
  mtext("b) Parasite Counts", 3, 1, at=2.9)
}
Figure9b()


## ------------------------------------------------------------------------------------------------
png("Figure9b.png", height= 360, width = 720)
Figure9b()
invisible(dev.off(dev.cur()))

