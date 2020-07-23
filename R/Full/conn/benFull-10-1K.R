# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t10_1KF   <- read.csv("ben-runConnFalseT10-1K.csv")

xrange = range(10:3011) 
yranges <- c(0.0,900)
yrange = range(yranges)

plot( main="Performance of Connectivity on (10-node) Full Forests, 1000 runs, conn=False"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.75)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.75)

lines( t10_1KF$n,t10_1KF$mean1e3
     , type = "o" 
     , col  = "blue"
     , pch  = 13  
     , lty  = 1 
     , lwd  = 1)  
