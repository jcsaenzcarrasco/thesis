
fullN50  <- read.csv("all-queries.csv")[c(3)]
fullN75  <- read.csv("all-queries.csv")[c(4)]
fullN101 <- read.csv("all-queries.csv")[c(5)]
fullN25  <- read.csv("all-queries.csv")[c(6)]

#semiN50  <- read.csv("all-queries.csv")[c(9)]
#semiN75  <- read.csv("all-queries.csv")[c(10)]
#semiN101 <- read.csv("all-queries.csv")[c(11)]
#semiN25  <- read.csv("all-queries.csv")[c(12)]

#seqN50  <- read.csv("all-queries.csv")[c(15)]
#seqN75  <- read.csv("all-queries.csv")[c(16)]
#seqN101 <- read.csv("all-queries.csv")[c(17)]
#seqN25  <- read.csv("all-queries.csv")[c(18)]

topN50  <- read.csv("all-queries.csv")[c(21)]
topN75  <- read.csv("all-queries.csv")[c(22)]
topN101 <- read.csv("all-queries.csv")[c(23)]
topN25  <- read.csv("all-queries.csv")[c(24)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0:1400) 

plot( main ="Full and Top FTs: Querying a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = "" # expression(paste("Time (", mu, "s)"))
#    , type = "l"          # line type (just straight)
#    , lty  = 2            # line pattern (dashed)
#    , lwd  = 2            # line width 
    , log  = ""
    , cex.lab=1.5, cex.axis=1.25, cex.main=1.5, cex.sub=1.5)
title(ylab=expression(paste("Time (", mu, "s)")), line=2, cex.lab=1.5)

# ======================================  full N50  L I N E  ============== 

lines(lowess(fullN50)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "magenta1"  
     , pch  = 0        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ======================================  top N50  L I N E  ============== 

lines(lowess(topN50)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue1"  
     , pch  = 7        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ======================================  fullN75   L I N E  ============= 

lines(lowess(fullN75) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "magenta2"
     , pch  = 1        # line character (triangles pointing down)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  topN75   L I N E  ============= 

lines(lowess(topN75) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue2"
     , pch  = 12       # line character (triangles pointing down)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  full N101   L I N E  ============== 

lines(lowess(fullN101 + 20) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "magenta3"
     , pch  = 2       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  top N101   L I N E  ============== 

lines(lowess(topN101)
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue3"
     , pch  = 9       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  full N25   L I N E  ============== 

lines(lowess(fullN25) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "magenta4"
     , pch  = 6        # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  top N25   L I N E  ============== 

lines(lowess(topN25)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue4"
     , pch  = 10        # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# =========================================  L  E  G  E  N  D  =============== 

legText = c( "Full N/2", "Full N3/4", "Full N+1", "Full N/4"
           , "Top  N/2", "Top  N3/4", "Top  N+1", "Top  N/4"
            ) 
legCol  = c("magenta1","magenta2", "magenta3", "magenta4"
           ,"blue1",   "blue2",    "blue3",    "blue4" )
legLTY  = c(1,1,1,1,1,1,1,1) 
legPCH  = c(0,1,2,6,7,12,9,10)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1.25)
