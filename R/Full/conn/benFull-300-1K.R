# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300_1KF  <- read.csv("ben-runConnFalseT300-1K.csv")

xrange = range(10:3011) 
yranges <- c(0,440)
yrange = range(yranges)

plot( main="Performance of Connectivity on (300-node) Full Forests, 1000 runs, conn=False"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.75)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.75)

lines( t300_1KF$n,t300_1KF$median1e3  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1
     , cex  = 1.75)  
