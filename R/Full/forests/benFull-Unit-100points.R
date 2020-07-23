# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

unit <- read.csv("ben-runUnitForest-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,0.02)
yrange = range(0:20)

plot( main="Performance of Creating (Unit) Full Forests"
    , xrange, yrange 
    , xlab = "", ylab = "" 
    , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.75)
# title(ylab=expression(paste("running time (" ~ m ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.75)

lines( unit$n,unit$median1e3
     , type = "o"      # line type 
     , col  = "salmon4"  
     , pch  = 5        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
