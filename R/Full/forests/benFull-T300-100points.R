# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("ben-runForT300-100points.csv")

xrange = range(10:3011) 
yranges <- c(80,860)
yrange = range(yranges)

plot( main="Performance of Creating (300-node) Full Forests"
    , xrange, yrange 
    , xlab = "" # "number of nodes"
    , ylab = "" 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.75)
# title(ylab=expression(paste("running time (" ~ m ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.75)

lines( t300$n,t300$median1e3
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 13        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
