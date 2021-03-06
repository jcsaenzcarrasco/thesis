# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("benFull-linksT300.csv")
t10  <- read.csv("benFull-linksT10.csv")
t2   <- read.csv("benFull-linksT2.csv")
unit <- read.csv("benFull-linksUnit.csv")

xrange = range(10:3010)
yrange = range(0:10500)

plot( main="Performance of Linking Full Forests"
#    , unit$mean
    , xrange, yrange
    , xlab = "" 
    , ylab = ""
    , type = "o"
    , col  = "blue4"
    , pch  = 8
    , lty  = 1
    , lwd  = 0
    , ylim = c(0,11000)
    , las  = 0
#    , log  = ""
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(xlab="number of nodes (sampling every 10 means)",cex.lab=1.75, line = 2.5,ylab="running time (m seconds)")

lines( unit$n, unit$mean1e3
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 13       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t2$n, t2$mean1e3
     , type = "o"      # line type 
     , col  = "darkseagreen4"  
     , pch  = 20       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t10$n, t10$mean1e3
     , type = "o"      # line type 
     , col  = "darkmagenta"  
     , pch  = 9        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t300$n, t300$mean1e3
     , type = "o"      # line type 
     , col  = "salmon4"  
     , pch  = 5        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("unit","2-node", "10-node", "300-node")           
legCol  = c("blue","darkseagreen4","darkmagenta","salmon4") 
legLTY  = c(1,1,1,1)  # (1,3,6,4) 
legLWD  = c(1,1,1,1)
legPCH  = c(13,20,9,5)   # c(NA,NA,NA,NA)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
