# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t2   <- read.csv("ben-runCutT2-100points.csv")
t300 <- read.csv("ben-runCutT300-100points.csv")
t10  <- read.csv("ben-runCutT10-100points.csv")
full <- read.csv("ben-runCutFullUnit-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,3.2)
yrange = range(yranges)

plot( main="Performance of Cutting Top Forests"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( full$n,full$median
     , type = "o"
     , col  = "bisque4"
     , pch  = 2 
     , lty  = 1 
     , lwd  = 1) 
lines( t300$n,t300$median  
     , type = "o" 
     , col  = "blue4"
     , pch  = 8  
     , lty  = 1 
     , lwd  = 1)  
lines( t10$n,t10$median 
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

legText = c("one-tree-forest","300-node","10-node","2-node") 
legCol  = c("bisque4","blue4","darkgreen","darkred") 
legLTY  = c(1,1,1,1) 
legLWD  = c(1,1,1,1)
legPCH  = c(2,8,1,3)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
