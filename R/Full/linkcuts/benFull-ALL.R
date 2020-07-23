# show the plot by calling <source ("ben-runForT10-11.R")> in the R Console

t1K_300  <- read.csv("benFull-linkcutsT300sizes-1000.csv")
t500_300 <- read.csv("benFull-linkcutsT300sizes-500.csv")
t300     <- read.csv("benFull-linkcutsT300sizes-300.csv")
t1K_10   <- read.csv("benFull-linkcutsT10sizes-1000.csv")
t500_10  <- read.csv("benFull-linkcutsT10sizes-500.csv")

xrange = range(10:3011) 
yranges <- c(0,0.25,0.35,3.4)
yrange = range(yranges)

plot( main="Performance of Linking & Cutting Full Forests"
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
title(xlab="number of nodes (sampling every 10 means)", line=2.5, cex.lab=1.25)

lines( t1K_300$n,t1K_300$mean   # or lowess (trees)  
     , type = "o"      # line type 
     , col  = "blue4"  
     , pch  = 8        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500_300$n,t500_300$mean       
     , type = "o"      # line type 
     , col  = "darkgreen"  
     , pch  = 1       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t300$n, t300$mean     
     , type = "o"      # line type 
     , col  = "darkred"  
     , pch  = 3       # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 

lines( t1K_10$n,t1K_10$mean
     , type = "o"      # line type 
     , col  = "blue"  
     , pch  = 13        # line character 
     , lty  = 1        # line pattern 
     , lwd  = 1)       # line width 
lines( t500_10$n,t500_10$mean
     , type = "o"      # line type 
     , col  = "darkseagreen4"  
     , pch  = 20       # line character 
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
      , cex = 1.2
      , bty = "n" )
