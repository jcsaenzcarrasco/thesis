# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

full <- read.csv("ben-runCutFullUnit.csv")

xrange = range(10:3011) 
yranges <- c(0,3.2)
yrange = range(yranges)

plot( main="Performance of Cutting (one-tree-forest) Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.25)

lines( full$n,full$mean
     , type = "o"
     , col  = "bisque4"
     , pch  = 2 
     , lty  = 1 
     , lwd  = 1) 
