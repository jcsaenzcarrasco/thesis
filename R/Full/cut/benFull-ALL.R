# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runCutT2.csv")
t300 <- read.csv("ben-runCutT300.csv")
t10  <- read.csv("ben-runCutT10.csv")
full <- read.csv("ben-runCutFullUnit.csv")

xrange = range(10:3011) 
yranges <- c(0,3.9)
yrange = range(yranges)

plot( main="Performance of Cutting Full Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.75)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.75)

lines( t300$n,t300$mean  
     , type = "o" 
     , col  = "blue"
     , pch  = 13  
     , lty  = 1 
     , lwd  = 1)  
lines( full$n,full$mean
     , type = "o"
     , col  = "salmon4"
     , pch  = 5 
     , lty  = 1 
     , lwd  = 1) 
lines( t10$n,t10$mean 
     , type = "o" 
     , col  = "darkseagreen"
     , pch  = 20 
     , lty  = 1 
     , lwd  = 1) 
lines( t2$n, t2$mean     
     , type = "o" 
     , col  = "darkmagenta" 
     , pch  = 6 
     , lty  = 1 
     , lwd  = 1)

legText = c("300-node","one-tree","10-node","2-node") 
legCol  = c("blue","salmon4","darkseagreen","darkmagenta") 
legLTY  = c(1,1,1,1) 
legLWD  = c(1,1,1,1)
legPCH  = c(13,5,20,6)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
