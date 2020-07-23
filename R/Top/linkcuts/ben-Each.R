# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t <- read.csv("ben-Each.csv")

xrange = range(1:500)
yrange = range(5:5900)

plot( main="Performance of Linking & Cutting Top Forests, per operation"
    , xlab = ""
    , ylab = ""  # see the "title" command below 
    , xrange, yrange
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of link-cut runs",line=2.5,cex.lab=1.25)

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

lines( t$n, t$eachLogEach 
     , type = "l"      # line type 
     , col  = "blue"  
     , pch  = 1        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 2)       # line width 


legText = c("10-node (500 runs)","linear","n log n") 
legCol  = c("darkgreen","red","blue") 
legLTY  = c(1,1,1) 
legLWD  = c(1,2,2)
legPCH  = c(1,NA,NA)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
