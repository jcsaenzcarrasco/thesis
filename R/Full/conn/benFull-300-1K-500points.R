# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300_1KF  <- read.csv("ben-runConnFalseT300-1K-500points.csv")
t300_1KT  <- read.csv("ben-runConnTrueT300-1K-500points.csv")

xrange = range(10:3011) 
yranges <- c(0.0,0.42)
yrange = range(yranges)

plot( main="Performance of Connectivity on (300-node) Full Forests, 1000 runs"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 500 medians)", line=2.5, cex.lab=1.25)

lines( t300_1KF$n,t300_1KF$median  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1)  
lines( t300_1KT$n,t300_1KT$median
     , type = "o"
     , col  = "bisque4"
     , pch  = 2 
     , lty  = 1 
     , lwd  = 1) 

legText = c("300-node,1000 runs,conn=False","300-node,1000 runs,conn=True")
 
legCol  = c("blue4","bisque4") 
legLTY  = c(1,1) 
legLWD  = c(1,1)
legPCH  = c(8,2)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
