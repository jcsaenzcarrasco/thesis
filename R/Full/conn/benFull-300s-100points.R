# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300_300F <- read.csv("ben-runConnFalseT300-300-100points.csv")
t300_500F <- read.csv("ben-runConnFalseT300-500-100points.csv")
t300_1KF  <- read.csv("ben-runConnFalseT300-1K-100points.csv")
t300_300T <- read.csv("ben-runConnTrueT300-300-100points.csv")
t300_500T <- read.csv("ben-runConnTrueT300-500-100points.csv")
t300_1KT  <- read.csv("ben-runConnTrueT300-1K-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,430)
yrange = range(yranges)

plot( main="Performance of Connectivity on (300-node) Full Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.75)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.75)

lines( t300_1KF$n,t300_1KF$median1e3  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1)  
lines( t300_1KT$n,t300_1KT$median1e3
     , type = "o"
     , col  = "bisque4"
     , pch  = 2 
     , lty  = 1 
     , lwd  = 1) 
lines( t300_500F$n,t300_500F$median1e3 
     , type = "o" 
     , col  = "darkgreen"
     , pch  = 1 
     , lty  = 1 
     , lwd  = 1) 
lines( t300_500T$n, t300_500T$median1e3     
     , type = "o" 
     , col  = "darkred" 
     , pch  = 3 
     , lty  = 1 
     , lwd  = 1)
lines( t300_300F$n,t300_300F$median1e3 
     , type = "o" 
     , col  = "blueviolet"
     , pch  = 7 
     , lty  = 1 
     , lwd  = 1) 
lines( t300_300T$n, t300_300T$median1e3     
     , type = "o" 
     , col  = "brown2" 
     , pch  = 14 
     , lty  = 1 
     , lwd  = 1)

legText = c("300-node,1000 runs,conn=False","300-node,1000 runs,conn=True",
            "300-node, 500 runs,conn=False","300-node, 500 runs,conn=True",
            "300-node, 300 runs,conn=False","300-node, 300 runs,conn=True") 
legCol  = c("blue4","bisque4","darkgreen","darkred","blueviolet","brown2") 
legLTY  = c(1,1,1,1,1,1) 
legLWD  = c(1,1,1,1,1,1)
legPCH  = c(8,2,1,3,7,14)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
