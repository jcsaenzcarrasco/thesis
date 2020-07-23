# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("ben-runForT300-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,0.42)
yrange = range(yranges)

plot( main="Performance of Creating (300-node) Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( t300$n,t300$median  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1)  
