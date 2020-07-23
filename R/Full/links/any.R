# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1KF  <- read.csv("benTop-connsFalseT10-1K.csv")
t500F <- read.csv("benTop-connsFalseT10-500.csv")

t1KT  <- read.csv("benTop-connsTrueT10-1K.csv")
t500T <- read.csv("benTop-connsTrueT10-500.csv")

xrange = range(10:160)  # (1500:1660) , (2850:3010) 
yrange = range(0:30)  # (260:320) , (390:470)  

plot( main="Performance for Connectivity Top Forests, per operation"
    , xrange, yrange 
    , xlab = "" 
    , ylab = "" # see title function below 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab=expression(paste("running time ("~ mu ~ " seconds)")), line=2.5, cex.lab=1.25)
#title(ylab=expression(paste("running time (1x" ~ 10^-4 ~ "seconds)")), line=2.4,cex.lab=1.25)
title(xlab="number of nodes", line=2.5, cex.lab=1.25)

lines( t1KT$n,t1KT$each1e6 
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t1KF$n,t1KF$each1e6 
     , type = "o"      # line type 
     , col  = "darkmagenta"  
     , pch  = 2        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t500T$n, t500T$each1e6
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500F$n, t500F$each1e6
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 1       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("10-node (1000 runs, connected==True)","10-node (1000 runs, connected==False)","10-node (500 runs, connected==True)","10-node (500 runs, connected==False)")           
legCol  = c("darkred","darkmagenta","blue4","blue") 
legLTY  = c(1,1,1,1) 
legLWD  = c(1,1,1,1)
legPCH  = c(3,2,8,1)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
