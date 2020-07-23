# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t <- read.csv("ben-Each-300.csv")

xrange = range(1:1000)
yrange = range(1:1410)

plot( main="Performance of individual link-cut over a 3,011-node Full forest"
    , xlab = ""
    , ylab = ""  # see the "title" command below 
    , xrange, yrange
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.75)
title(xlab="number of link-cut runs",line=2.5,cex.lab=1.75)

lines( t$n, t$each1e6 
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t$n, t$m 
     , type = "l"      # line type 
     , col  = "red"  
     , pch  = 1        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 2)       # line width 

legText = c("300-node (1000 runs)","linear reference") 
legCol  = c("darkgreen","red") 
legLTY  = c(1,1) 
legLWD  = c(1,2)
legPCH  = c(1,NA)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
