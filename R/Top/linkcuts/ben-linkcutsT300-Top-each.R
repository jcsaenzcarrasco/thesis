# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K  <- read.csv("benTop-linkCutsT300-1K.csv")
t500 <- read.csv("benTop-linkCutsT300-500.csv")
t300 <- read.csv("benTop-linkCutsT300-300.csv")

xrange = range(300:3020)  
yrange = range(60:1100)

plot( main="Performance of Linking & Cutting Top Forests, per operation"
#    , t1K$n, t1K$each1e6
    , xrange, yrange 
    , xlab = ""  # number of nodes (10 each point)"
    , ylab = ""  # see title function below 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
#    , log  = ""
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.2)
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.2)

lines( t1K$n,t1K$each1e6    # or lowess (trees)  
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 2)       # line width 

lines( t500$n, t500$each1e6       
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t300$n, t300$each1e6       
     , type = "o"      # line type 
     , col  = "peachpuff4"  
     , pch  = 13       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 


legText = c("300-node (1000 runs)","300-node (500 runs)","300-node (300 runs)")           
legCol  = c("darkred","blue4","peachpuff4") 
legLTY  = c(1,1,1) 
legLWD  = c(2,1,1)
legPCH  = c(3,8,13)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
