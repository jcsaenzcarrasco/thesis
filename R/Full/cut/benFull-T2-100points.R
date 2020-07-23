# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runCutT2-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,0.51)
yrange = range(yranges)

plot( main="Performance of Cutting (2-node) Full Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( t2$n, t2$median     
     , type = "o" 
     , col  = "darkmagenta" 
     , pch  = 6 
     , lty  = 1 
     , lwd  = 1)
