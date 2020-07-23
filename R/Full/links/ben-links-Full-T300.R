# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t300 <- read.csv("ben-runLinkT300-301-3011-11.csv")

xrange = range(2901:2921)
yrange = range(12:22)

plot( main="Performance of Linking 300-node Full Forests"
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
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(xlab="number of nodes (sampling every 10 means)",cex.lab=1.75, line = 2.5,ylab="running time (m seconds)")

lines( t300$n, t300$median1e3 
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t300$n, t300$mean1e3 
     , type = "o"      # line type 
     , col  = "salmon4"  
     , pch  = 5        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t300$n, t300$meanStdDevUp 
     , type = "o"      # line type 
     , col  = "darkmagenta"  
     , pch  = 1        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 



legText = c("mean + std dev","mean","median")
legCol  = c("darkmagenta","salmon4","blue4") 
legLTY  = c(1,1,1)
legLWD  = c(1,1,1)
legPCH  = c(1,5,8)  

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
