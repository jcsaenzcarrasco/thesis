
bothFull <- read.csv("all-creation.csv")[c(4)]
bothSemi <- read.csv("all-creation.csv")[c(8)]
bothSeq  <- read.csv("all-creation.csv")[c(12)]
bothTop  <- read.csv("all-creation.csv")[c(16)]

# =======================================  P L O T   L A Y E R  ============ 

xrange = range(0:50)
yrange = range(0:45) 

plot( main ="All-FTs: Creating a forest of 50-node trees" 
    , xrange, yrange
    , xlab = "Number of Operations (x10,000)"
    , ylab = "Time(s)"
    , type = "l"          # line type (just straight)
    , lty  = 2            # line pattern (dashed)
    , lwd  = 2            # line width 
    , log  = ""
    , cex.lab=1.5, cex.axis=1.25, cex.main=1.5, cex.sub=1.5)


# ======================================  BothFull  L I N E  ============== 

lines(lowess(bothFull)            # lowess (trees)  
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkmagenta"  
     , pch  = 0        # line character (triangles pointing up)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)       # line width 

# ======================================  BothSemi   L I N E  ============= 

lines(lowess(bothSemi) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue"
     , pch  = 7        # line character (triangles pointing down)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  BothSeq   L I N E  ============== 

lines(lowess(bothSeq) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "darkgreen"
     , pch  = 12       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# ======================================  BothTop   L I N E  ============== 

lines(lowess(bothTop) 
     , type = "o"      # line type (straight and bullets as circles)
     , col  = "blue4"
     , pch  = 14       # line character (diamonds)
     , lty  = 1        # line pattern (solid)
     , lwd  = 2)

# =========================================  L  E  G  E  N  D  =============== 

legText = c( "Full", "Semi", "Seq", "Top" )           
legCol  = c("darkmagenta","blue", "darkgreen", "blue4") 
legLTY  = c(1,1,1,1) 
legPCH  = c(0,7,12,14)

legend( "topleft"
      , legend = legText
      , col = legCol
      , lty = legLTY    # line pattern (dashed, solid)
      , pch = legPCH    # line character (none i.e. NA, solid circle i.e. 16)
      , lwd = 2
      , cex = 1.25)
