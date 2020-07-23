# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K_300  <- read.csv("benFull-linkcutsT300sizes-1000-100points.csv")
t500_300 <- read.csv("benFull-linkcutsT300sizes-500-100points.csv")
t300     <- read.csv("benFull-linkcutsT300sizes-300-100points.csv")
t1K_10   <- read.csv("benFull-linkcutsT10sizes-1000-100points.csv")
t500_10  <- read.csv("benFull-linkcutsT10sizes-500-100points.csv")

xrange = range(10:3011) 
#yranges <- c(0,0.25,0.35,3.4)
#yrange = range(0:3300)
yrange = range(0:3300)

plot( main="Performance of Linking & Cutting Full Forests, per operation"
    , xrange, yrange 
    , xlab = "" # "number of nodes"
    , ylab = "" 
    , type = "o"
    , col  = "darkred"
    , pch  = 3
    , lty  = 1
    , lwd  = 0
    , cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75)
# title(ylab="running time (seconds)", line=2.5, cex.lab=1.75)
title(ylab=expression(paste("running time (" ~ mu ~ "seconds)")), line=2.5, cex.lab=1.75)
title(xlab="number of nodes (sampling every 100 medians)", line=2.5, cex.lab=1.75)

lines( t1K_300$n,t1K_300$medianEach1e6
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500_300$n,t500_300$medianEach1e6  
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t300$n, t300$medianEach1e6
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t1K_10$n,t1K_10$medianEach1e6
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 13        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500_10$n,t500_10$medianEach1e6
     , type = "o"      # line type 
     , col  = "darkseagreen4"  
     , pch  = 16       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

legText = c("10-node (1000 runs)","10-node (500 runs)","300-node (1000 runs)", "300-node (500 runs)","300-node (300 runs)")           
legCol  = c("blue","darkseagreen4","blue4","darkgreen","darkred") 
legLTY  = c(1,1,1,1,1) 
legLWD  = c(1,1,1,1,1)
legPCH  = c(13,20,8,1,3)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    
      , pch = legPCH
      , lwd = legLWD
      , cex = 1.75
      , bty = "n" )
