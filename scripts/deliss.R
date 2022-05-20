delissage <- function(s){
  # s vector to be delisse: sum of the last 7 days
  # t corresponding end dates: last date of the interval
  ns <- length(s)
  
  # Initialize new vector; 
  # initially set the first 6 points to zero
  w <- c(rep(0, 7), rep(NA, ns - 1))
  
  # Label days along the week
  values <- rep(1:7, ceiling(length(w)/7))[1:length(w)] 
  
  # Loop on values
  # (Cannot be done in vector form it seems)
  for(i in 1:(ns - 1)){
    w[i + 7] <- s[i + 1] - s[i] + w[i]
  }
  
  # Find the minimum values for each of the first 7 points
  # such that there is no negative value
  for(j in 1:7){ # Days along a week
    # What is the minimal inferred value
    mj <- min(w[values == j], na.rm = TRUE)
    if(mj < 0){ 
      # If there are negative values, set the new value, 
      # but if there are no negative values, keep 0
      w[j] <- - min(w[values == j])
    }
  }
  
  # Repeat loop
  for(i in 1:(ns - 1)){
    w[i + 7] <- s[i + 1] - s[i] + w[i]
  }
  
  # We may still have some cases to assign
  difference <- s[1] - sum(w[1:7])
  
  stopifnot(difference >= 0)
  
  if(difference > 0){
    # Compute the distribution of numbers of tests along the days of the week
    aggn <- aggregate(w, by = list(values), FUN = sum)
    sumx <- sum(aggn$x)
    if(sumx > 0){
      aggn$p <- aggn$x / sumx
    }else{ # Can be = 0...!
      aggn$p <- aggn$x
    }
    
    # Assign the left-over values according to this distribution
    newn <- rep(NA, 7)
    newn[1:6] <- round(aggn[1:6, "p"] * difference)
    newn[7] <- difference - sum(newn[1:6])
    # If we had too many tests in the previous days, and the last one ends up negative,
    # remove values before
    while(newn[7] < 0){
      # Choose one day at random
      k <- sample(1:6, size = 1)
      if(newn[k] > 0){ # If we can remove a day
        newn[k] <- newn[k] - 1 # Remove it
        newn[7] <- difference - sum(newn[1:6]) # Recompute newn[7]
      }
    }
    
    # If we had too many tests in the previous days, and the last one ends up negative,
    # remove values before
    while(newn[7] < 0){
      # Choose one day at random
      k <- sample(1:6, size = 1)
      if(newn[k] > 0){ # If we can remove a day
        newn[k] <- newn[k] - 1 # Remove it
        newn[7] <- difference - sum(newn[1:6]) # Recompute newn[7]
      }
    }
    
    stopifnot(all(newn >= 0)) # Security check
    
    # Add them to the w
    w[1:7] <- w[1:7] + newn
  
    # Repeat loop one last time
    for(i in 1:(ns - 1)){
      w[i + 7] <- s[i + 1] - s[i] + w[i]
    }
  }
  
  # Quality control
  if(!all(sliding.window(w, pos = 7, FUN = sum) == c(rep(NA, 6), s), na.rm = TRUE)) stop("Deliss problem")
  return(w)
}


# Function to delisser multiple columns and add date
# Need to be careful with dates
delissWithDate <- function(dat, cols){
  # dat our dataset, with time columns
  # cols columns to be "delisee", as vector of chars
  # Check that all dates are consecutive
  dates <- as.Date(as.Date(substring(dat$semaine, 12, 21)))
  stopifnot(all(as.numeric(diff(dates)) == 1))
  
  inits <- as.data.frame(matrix(NA, ncol = ncol(dat), nrow = 6))
  names(inits) <- names(dat)
  # Fill in geographic information, separately for dep and reg
  if(is.element("reg", names(dat))){
    inits$reg <- unique(dat$reg)
    inits$nom_reg <- unique(dat$nom_reg)
  }
  if(is.element("dep", names(dat))){
    inits$dep <- unique(dat$dep)
    inits$nom_dep <- unique(dat$nom_dep)
  }
  
  out <- rbind(inits, dat)
  
  for(col in cols){
    out[, paste0(col, ".dl")] <- delissage(dat[, col])
    out[, paste0(col, ".sum7_mid")] <- c(rep(NA, 3), dat[, col], rep(NA, 3))
  }
  out$dateDeliss <- c(dates[1] + (-6 : -1), dates)
  out 
}



