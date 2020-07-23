# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runForT2-100points.csv")
t300 <- read.csv("ben-runForT300-100points.csv")
t10  <- read.csv("ben-runForT10-100points.csv")
unit <- read.csv("ben-runUnitForest-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,0.42)
yrange = range(yranges)

plot( main="Performance of Creating Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# title(ylab=expression(paste("running time (" ~ m ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( t300$n,t300$median
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t10$n,t10$median       
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t2$n, t2$median     
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( unit$n,unit$median
     , type = "o"      # line type 
     , col  = "bisque4"  
     , pch  = 2        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

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
