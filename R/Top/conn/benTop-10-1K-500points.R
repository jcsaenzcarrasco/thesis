# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t10_1KF   <- read.csv("ben-runConnFalseT10-1K-500points.csv")
t10_1KT   <- read.csv("ben-runConnTrueT10-1K-500points.csv")

xrange = range(10:3011) 
yranges <- c(0,450)
yrange = range(yranges)

plot( main="Performance of Connectivity on (10-node) Top Forests, 1000 runs"
    , xrange, yrange 
    , xlab = "" , ylab = "" , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (m seconds)", line=2.5, cex.lab=1.25)
# ylab=expression(paste("running time (" ~ m ~ "seconds)"))
title(xlab="number of nodes (sampling every 500 medians)", line=2.5, cex.lab=1.25)

lines( t10_1KT$n,t10_1KT$median1e3
     , type = "o"
     , col  = "salmon4"
     , pch  = 5 
     , lty  = 1 
     , lwd  = 1) 
lines( t10_1KF$n,t10_1KF$median1e3
     , type = "o" 
     , col  = "blue"
     , pch  = 13  
     , lty  = 1 
     , lwd  = 1)  

legText = c("connected=True","connected=False")

legCol  = c("salmon4","blue") 
legLTY  = c(1,1) 
legLWD  = c(1,1)
legPCH  = c(5,13)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
