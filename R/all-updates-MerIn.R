fullMerNo <- read.csv("all-updates.csv")[c(1)]
fullMerIn <- read.csv("all-updates.csv")[c(2)]
fullIns   <- read.csv("all-updates.csv")[c(3)]

semiMerNo <- read.csv("all-updates.csv")[c(5)]
semiMerIn <- read.csv("all-updates.csv")[c(6)]
semiIns   <- read.csv("all-updates.csv")[c(7)]

seqMerNo <- read.csv("all-updates.csv")[c(9)]
seqMerIn <- read.csv("all-updates.csv")[c(10)]
seqIns   <- read.csv("all-updates.csv")[c(11)]

topMerNo <- read.csv("all-updates.csv")[c(13)]
topMerIn <- read.csv("all-updates.csv")[c(14)]
topIns   <- read.csv("all-updates.csv")[c(15)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0:1242683) 

plot( main ="All-FTs: Appending with Searching in a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = expression(paste("Time (", mu, "s)"))
    , type = "l"          # line type (just straight)
    , lty  = 2            # line pattern (dashed)
    , lwd  = 2            # line width 
    , log  = "")


# ==================================  full updates  L I N E s ============== 

lines(lowess(fullMerIn + 20000)         # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "magenta2"  
     , pch  = 1        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ==================================  semi updates  L I N E s ============== 

lines(lowess(semiMerIn)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue2"  
     , pch  = 12        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ==================================  seq updates  L I N E s ============== 

lines(lowess(seqMerIn)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "green3"  
     , pch  = 21        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ==================================  top updates  L I N E s ============== 

lines(lowess(topMerIn)          # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "red2"  
     , pch  = 16        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# =========================================  L  E  G  E  N  D  =============== 

legText = c( "Full", "Semi", "Seq", "Top" )
legCol  = c("magenta2", "blue2", "green3", "red2") 
legLTY  = c(1,1,1,1) 
legPCH  = c(1,12,21,16)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1)
