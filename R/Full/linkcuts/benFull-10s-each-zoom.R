# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K_10   <- read.csv("benFull-linkcutsT10sizes-1000.csv")
t500_10  <- read.csv("benFull-linkcutsT10sizes-500.csv")

xrange = range(2850:3010) 
yrange = range(3050:3350)

plot( main="Performance of Linking & Cutting Full Forests, per operation"
    , xrange, yrange 
    , xlab = "" # "number of nodes"
    , ylab = "" 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
# title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.25)

lines( t1K_10$n,t1K_10$each1e6
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 13        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500_10$n,t500_10$each1e6
     , type = "o"      # line type 
     , col  = "darkseagreen4"  
     , pch  = 20       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("10-node (1000 runs)","10-node (500 runs)")           
legCol  = c("blue","darkseagreen4") 
legLTY  = c(1,1) 
legLWD  = c(1,1)
legPCH  = c(13,20)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
