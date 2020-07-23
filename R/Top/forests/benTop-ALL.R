# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runForT2.csv")
t300 <- read.csv("ben-runForT300.csv")
t10  <- read.csv("ben-runForT10.csv")
unit <- read.csv("ben-runUnitForest.csv")

xrange = range(10:3011) 
yranges <- c(0,0.42)
yrange = range(yranges)

plot( main="Performance of Creating Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.25)

lines( t300$n,t300$mean  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1)  
lines( t10$n,t10$mean 
     , type = "o" 
     , col  = "darkgreen"
     , pch  = 1 
     , lty  = 1 
     , lwd  = 1) 
lines( t2$n, t2$mean     
     , type = "o" 
     , col  = "darkred" 
     , pch  = 3 
     , lty  = 1 
     , lwd  = 1)
lines( unit$n,unit$mean
     , type = "o"
     , col  = "bisque4"
     , pch  = 2 
     , lty  = 1 
     , lwd  = 1) 

legText = c("300-node","10-node","2-node", "unit")           
legCol  = c("blue4","darkgreen","darkred","bisque4") 
legLTY  = c(1,1,1,1,1) 
legLWD  = c(1,1,1,1,1)
legPCH  = c(8,1,3,2)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
