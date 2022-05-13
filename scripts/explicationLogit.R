# Load color package
library(MetBrewer)

# Define logistic function
logisticFunc <- function(x, s = 1, x0 = 0){
  1 / (1 + exp(-s * (x - x0)))
}

# Define logit function
logit <- function(p){
  log(p / (1 - p))
}

# Define linear function
# (yes it's a bit ridiculous)
linear <- function(x){x}

# Generate data
npts <- 1001 # nb of points
x <- seq(0, 200, length.out = npts)
y <- logisticFunc(x, s = 0.09, x0 = 100)

# y graduations
yy1 <- c(0.001, 0.01, 0.1)
yy <- sort(c(yy1, 0.5, 1 - yy1))
yy2 <- c(10^-4, yy, 1- 10^-4) # add extremes

# Define color palette
pal2 <- met.brewer("Hiroshige", length(yy2) - 1, "continuous")#[c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]

# Define function to plot the curve on the two scales
plotCurve <- function(thefunc){
  # thefunc: logit or linear
  par(las = 1, xpd = FALSE)
  # Initialize plot
  plot(x, thefunc(y), axes = FALSE, type = "n", ylab = "y", yaxs = "i")
  axis(1)#, pos = thefunc(par("usr")[1]))
  axis(2, at = thefunc(yy), labels = yy, pos = x[1])
  
  for(i in 1:(length(yy2) - 1)){
    # Shade rectangles
    #  polygon(x = c(x, rev(x), x[1]), 
    #          y = thefunc(c(rep(yy2[i], npts), rep(yy2[i+1], npts), yy2[i])), 
    #          border = gray(1, 1), col = adjustcolor(pal2[i], 0.1))
  }

  # points(x, thefunc(y), col = "black", type = "l", lwd = 4)
  
  # Add graduations (with colors)
  for(i in seq_along(yy)){
    lines(x, rep(thefunc(yy[i]), length(x)), col = pal2[i])
  }

  # Discretize the y values along our graduations
  ycats <- cut(y, yy2, labels = FALSE)
  
  par(xpd = TRUE)
  for(i in unique(ycats)){
    # Subset of the data for this category value
    ii <- which(ycats == i)
    X <- x[ii]
    Y <- thefunc(y[ii])
    # Plot the data, colored by category
    segments(x0 = head(X, -1), y0 = head(Y, -1), 
             x1 = tail(X, -1), y1 = tail(Y, -1), 
             col = pal2[i], lwd = 5)
  }
  par(xpd = FALSE)
}

## Plot

# PNG characteristics
wpng <- 6
hpng <- 6
respng <- 300

# Logit version
png("../pics/explication-logit_1.png", 
    width = wpng, height = hpng, res = respng, units = "in")
plotCurve(logit)
title("Les mêmes données en échelle logit")
dev.off()

# Linear version
png("../pics/explication-logit_0.png", 
    width = wpng, height = hpng, res = respng, units = "in")
plotCurve(linear)
title("Les données en échelle linéaire")
dev.off()

# Convert the plot to GIF
system("convert -delay 200 -loop 1 ../pics/explication-logit*.png ../pics/explication_logit.gif")
