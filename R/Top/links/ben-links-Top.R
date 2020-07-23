# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("benTop-linksT300.csv")
t10  <- read.csv("benTop-linksT10.csv")
t2   <- read.csv("benTop-linksT2.csv")
unit <- read.csv("benTop-linksUnit.csv")

xrange = range(10:3010)
yrange = range(0:4)

plot( main="Performance of Linking Top Forests"
#    , unit$mean
    , xrange, yrange
    , xlab = "" 
    , ylab = ""
    , type = "o"
    , col  = "blue4"
    , pch  = 8
    , lty  = 1
    , lwd  = 0
#    , log  = ""
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(xlab="number of nodes (sampling every 10 means)",cex.lab=1.25, line = 2.5,ylab="running time (seconds)")

lines( unit$n, unit$mean       
     , type = "l"      # line type 
     , col  = "blue4"  
     , pch  = 8       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 3)       # line width 
lines( t2$n, t2$mean         # or lowess (value)  
     , type = "l"      # line type 
     , col  = "darkgreen"  
     , pch  = 1       # line character 
     , lty  = 3        # line pattern 
     , lwd  = 3)       # line width 
lines( t10$n, t10$mean 
     , type = "l"      # line type 
     , col  = "darkred"  
     , pch  = 3        # line character 
     , lty  = 6        # line pattern 
     , lwd  = 3)       # line width 
lines( t300$n, t300$mean 
     , type = "l"      # line type 
     , col  = "bisque4"  
     , pch  = 3        # line character 
     , lty  = 4        # line pattern 
     , lwd  = 3)       # line width 

legText = c("unit","2-node", "10-node", "300-node")           
legCol  = c("blue4","darkgreen","darkred","bisque4") 
legLTY  = c(1,3,6,4) 
legLWD  = c(3,3,3,3)
legPCH  = c(NA,NA,NA,NA)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
