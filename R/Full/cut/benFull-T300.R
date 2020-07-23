# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("ben-runCutT300.csv")

xrange = range(10:3011) 
yranges <- c(0,3.9)
yrange = range(yranges)

plot( main="Performance of Cutting (300-node) Full Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.25)

lines( t300$n,t300$mean
     , type = "o" 
     , col  = "blue"
     , pch  = 13  
     , lty  = 1 
     , lwd  = 1)  
