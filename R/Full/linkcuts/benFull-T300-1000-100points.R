# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K_300  <- read.csv("benFull-linkcutsT300sizes-1000-100points.csv")

xrange = range(10:3011) 
yranges <- c(0,0.25,0.35,1.5)
yrange = range(yranges)

plot( main="Performance of Linking & Cutting (300-node) Full Forests, 1000 runs"
    , xrange, yrange 
    , xlab = "" # "number of nodes"
    , ylab = "" 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
title(ylab="running time (seconds)", line=2.5, cex.lab=1.25)
# title(ylab=expression(paste("running time (" ~ m ~ "seconds)")), line=2.5, cex.lab=1.25)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.25)

lines( t1K_300$n,t1K_300$median
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
