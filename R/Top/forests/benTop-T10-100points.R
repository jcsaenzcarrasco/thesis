# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t10  <- read.csv("ben-runForT10-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,75)
yrange = range(yranges)

plot( main="Performance of Creating (10-node) Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.25)
# title(ylab=expression(paste("running time (" ~ m ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( t10$n,t10$median1e3  
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
