# "Unfold" window-summed data

unwindow <- function(v, winwdt = 7, pos = 7, ave = FALSE){
  # v: vector to unfold
  # winwdt: width of the window
  # pos: position of the focal element
  # ave: whether this is an average, FALSE = sum
  
  ## For the moment, pos = 7 only
  # Initialize new vector
  y <- rep(NA, length(v))
  
  if(ave){ # If average over window width, 
    # Convert into sum
    v <- winwdt*v
  }
  
  # Assumption: all values of the 1st element and the previous ones are equal to the first value
  y[1] <- v[1]/winwdt
  y[2] <- v[2] - 6*y[1]
#  y[3] <- v[3] - y[2] - 5*y[1]
#  y[4] <- v[4] - y[2:3] - 4*y[1]
#  y[5] <- v[5] <- y[2:4] - 3*y[1]
  for(i in 3:winwdt){
    y[i] <- v[i] - sum(y[2:(i-1)]) - (winwdt - i + 1)*y[1]
  }
  for(i in (winwdt+1):length(v)){
    y[i] <- v[i] - sum(y[(i - winwdt + 1):(i-1)])
  }
  y
}


# Function to compute a sliding window 
sliding.window <- function(v, winwdt = 7, pos = 4, na.rm = TRUE){
  # v vector to be averaged/summed
  # winwdt width of the window 
  # pos position of the focal day in the window
  # FUN function to apply
  n <- length(v)
  # Initialize output vector
  out <- 0 * v + (-1)
  out[1:(pos-1)] <- NA
  out[(n + 1 - winwdt + pos) : n] <- NA
  
  for(i in pos : (n - winwdt + pos)){
    out[i] <- mean(v[(i - pos + 1):(i + winwdt - pos)], na.rm = na.rm)
  }
  return(out[1:n])
}


vv <- 1+ rpois(100, lambda = 100)
vvs <- sliding.window(vv, pos = 7)
vvs <- vvs[7:length(vvs)]
yy <- unwindow(vvs, ave = TRUE)
plot(vvs, yy)
abline(a = 0, b = 1)
yys <- sliding.window(yy, pos = 7)
plot(vvs, yys)
