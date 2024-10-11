params <-
list(remakeFigs = FALSE)

## ----eval=FALSE----------------------------------------------------------------------------------
## devtools::install_github("dd-harp/ramp.falciparum")
## install.packages("ramp.falciparum")


## ------------------------------------------------------------------------------------------------
library(knitr)
library(viridis)






## ----eval = params$remakeFigs--------------------------------------------------------------------
print("Making Figure 3")
purl("Figure3.Rmd", "Figure3.R")
png("Figure3.png", height= 320, width = 720)
par(mfrow = c(1,2))
source("Figure3.R")
invisible(dev.off(dev.cur()))


## ----Fig4, eval = params$remakeFigs--------------------------------------------------------------
print("Making Figure 4")
purl("Figure4.Rmd", "Figure4.R")
png("Figure4a.png", height= 1080, width = 720)
source("Figure4.R")
invisible(dev.off(dev.cur()))


## ----Fig5, eval= params$remakeFigs---------------------------------------------------------------
print("Making Figure 5")
purl("Figure5.Rmd", "Figure5.R")
png("Figure5.png", height= 400, width = 720)
source("Figure5.R")
invisible(dev.off(dev.cur()))


## ----Fig6, eval= params$remakeFigs---------------------------------------------------------------
print("Making Figure 6")
purl("Figure6.Rmd", "Figure6.R")
png("Figure6.png", height=840, width=560)
par(mfrow = c(3,1))
source("Figure6.R")
invisible(dev.off(dev.cur()))


## ----Fig7, eval= params$remakeFigs---------------------------------------------------------------
print("Making Figure 7")
purl("Figure7.Rmd", "Figure7.R")
png("Figure7.png", height=1080, width=560)
par(mfrow = c(3,1))
source("Figure7.R")
invisible(dev.off(dev.cur()))


## ----Fig8, eval= params$remakeFigs---------------------------------------------------------------
print("Making Figure 8")
purl("Figure8.Rmd", "Figure8.R")
png("Figure8.png", height=360, width=720)
source("Figure8.R")
invisible(dev.off(dev.cur()))


## ----Fig9, eval= params$remakeFigs---------------------------------------------------------------
print("Making Figure 9")
purl("Figure9.Rmd", "Figure9.R")
png("Figure9.png", height=720, width=720)
par(mfrow = c(2,1))
source("Figure9.R")
invisible(dev.off(dev.cur()))


## ----Fig10, eval= params$remakeFigs--------------------------------------------------------------
print("Making Figure 10")
purl("Figure10.Rmd", "Figure10.R")
png("Figure10.png", height=360, width=720)
source("Figure10.R")
invisible(dev.off(dev.cur()))

