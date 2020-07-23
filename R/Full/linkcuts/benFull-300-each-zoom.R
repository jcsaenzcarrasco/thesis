# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K_300  <- read.csv("benFull-linkcutsT300sizes-1000.csv")
t500_300 <- read.csv("benFull-linkcutsT300sizes-500.csv")
t300     <- read.csv("benFull-linkcutsT300sizes-300.csv")

xrange = range(2400:3010)  # (300:460), (1500:1660)
yrange = range(10:1800)  # (570:820) , (80:170)  (730:840)   , (1600:1800)

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

lines( t300$n, t300$each1e6
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("300-node (1000 runs)", "300-node (500 runs)","300-node (300 runs)")           
legCol  = c("blue4","darkgreen","darkred") 
legLTY  = c(1,1,1) 
legLWD  = c(1,1,1)
legPCH  = c(8,1,3)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.2
      , bty = "n" )
