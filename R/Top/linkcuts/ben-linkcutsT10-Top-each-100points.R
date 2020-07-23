# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K  <- read.csv("ben-linkCutsT10-1K-each-100points.csv")
t500 <- read.csv("ben-linkCutsT10-500-each-100points.csv")

xrange = range(10:3000)
yrange = range(20:1800)

plot( main="Performance of Linking & Cutting Top Forests, per operation"
    , xlab = ""
    , ylab = ""  # see the "title" command below 
    , xrange, yrange
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
#    , log  = ""
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)",line=2.5,cex.lab=1.25)

lines( t1K$n, t1K$median1e6        # or lowess (trees)  
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t500$n, t500$median1e6       
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 2       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("10-node (1000 runs)","10-node (500 runs)") 
legCol  = c("darkgreen","blue") 
legLTY  = c(1,1) 
legLWD  = c(1,1)
legPCH  = c(1,2)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
