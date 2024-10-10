## -------------------------------------------------------------------------------------------------
#if(redo == TRUE){
  aa = seq(5, 5*365, by = 5)
  pr = sapply(aa, d_parasite_detect, FoIpar=foiP3)
  PR = data.frame(pr=as.vector(pr))
#  write.table(PR, "pr.txt") 
#}
  
moi = meanMoI(aa, foiP3)

#pr = read.table("pr.txt")$pr

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

plot(aa, xct, type = "l", ylim = c(0,1),
     xlab = "a - Cohort Age",
     ylab = expression(hat(p)[tau](a)))
lines(aa, aprx, col = "salmon")
#lines(aa, (aprx + aprx1)/2, col = "darkgreen")
#lines(aa, aprx1, col = "blue")


#plot(aa, xct-aprx, type = "l")
#segments(0,0, max(aa), 0)



## -------------------------------------------------------------------------------------------------
foiP3$hbar = 5/365

thou = 0:730
daoi <- dAoI(thou, 5*365, foiP3)

prZ = FQ(thou, 5*365, foiP3)

scl = max(prZ)/max(daoi)

par(mar = c(5,4,4,4))
plot(thou, prZ, type = "l", col = "darkorange4", lwd=2,
     xlab = expression(list(alpha, "Age of Infection")), 
     ylab = expression(F[Q](alpha)))

lines(thou, daoi*scl, type = "l", col = "darkblue") 
axis(4, pretty(daoi)*scl, pretty(daoi), col = "darkblue")
mtext(expression(f[A](alpha)), 4, 3)


