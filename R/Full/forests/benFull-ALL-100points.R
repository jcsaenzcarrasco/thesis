# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runForT2-100points.csv")
t300 <- read.csv("ben-runForT300-100points.csv")
t10  <- read.csv("ben-runForT10-100points.csv")
unit <- read.csv("ben-runUnitForest-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,850)
yrange = range(yranges)

plot( main="Performance of Creating Full Forests"
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
lines( t10$n,t10$median1e3  
     , type = "o"      # line type 
     , col  = "darkseagreen"  
     , pch  = 16       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t2$n, t2$median1e3 
     , type = "o"      # line type 
     , col  = "darkmagenta"  
     , pch  = 6       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( unit$n,unit$median1e3
     , type = "o"      # line type 
     , col  = "salmon4"  
     , pch  = 5        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("300-node","10-node","2-node", "unit")           
legCol  = c("blue","darkseagreen4","darkmagenta","salmon4") 
legLTY  = c(1,1,1,1,1) 
legLWD  = c(1,1,1,1,1)
legPCH  = c(13,16,6,5)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
