# Load raw data 
datBrutes <- read.csv("../data/dataBrutes.csv", sep = ";")

#head(datBrutes)

# ---
# Function to compute a sliding window 
sliding.window <- function(v, winwdt = 7, pos = 4, na.rm = TRUE, FUN = mean){
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
    out[i] <- FUN(v[(i - pos + 1):(i + winwdt - pos)], na.rm = na.rm)
  }
  return(out[1:n])
}
# ---


#region <- "Île-de-France"
#checkSums <- FALSE
#diagnosticPlot <- FALSE

delissage <- function(region, checkSums = FALSE, diagnosticPlot = FALSE){
  # Subset of data for this region
  tmpL <- dat.Regions[which(dat.Regions$reg_name == region), ]
  tmpB <- datBrutes[which(datBrutes$REGION == unique(tmpL$reg)), ]
  
  # head(tmpB)
  # head(tmpL)
  
  # Prepare merge:
  # Add information in the column names to be able to merge
  names(tmpB) <- c("date", "REGION", "nb_pos_brut", "nb_C0_brut", "nb_C1_brut")
  tmpB$date <- as.Date(tmpB$date) # Change to date to match the other data
  any(is.element(tmpB$date, tmpL$date2))
  
  # Make sure data are ordered chronologically
  all(diff(as.Date(tmpB$date)) == 1)
  all(diff(as.Date(tmpL$date2)) == 1)
  
  if(checkSums){
    # Check if summed raw data matches the summed data
    # Sum raw data
    tmpB$nb_C0.7j <- sliding.window(tmpB$nb_C0, pos = 7, FUN = sum)
    tmpB$nb_C1.7j <- sliding.window(tmpB$nb_C1, pos = 7, FUN = sum)
    
    if(diagnosticPlot){
      plot(as.Date(tmpB$date), tmpB$nb_C1.7j)
      points(as.Date(tmpB$date), tmpB$nb_C0.7j)
      
      points(as.Date(tmpL$date2), tmpL$nb_C0, col = 2, cex = 0.8)
      points(as.Date(tmpL$date2), tmpL$nb_C1, col = 2, cex = 0.8)
    }
    
    itv <- (1:7)
    print(tmpL$nb_C0[itv] - tmpB$nb_C0.7j[itv+6])
    print(tmpL$nb_C1[itv] - tmpB$nb_C1.7j[itv+6])
    
  }
  
  # Merge the datasets
  tmp <- merge(tmpL, tmpB, by.x = "date2", by.y = "date", all = TRUE)
  
  # head(tmp)
  
  # Delissage (dl)
  # Create columns
  tmp$nb_C0_dl <- NA
  tmp$nb_C1_dl <- NA
  tmp$nb_pos_dl <- NA
  # Initialize columns
  tmp$nb_C0_dl[1:7] <- tmp$nb_C0_brut[1:7]
  tmp$nb_C1_dl[1:7] <- tmp$nb_C1_brut[1:7]
  tmp$nb_pos_dl[1:7] <- tmp$nb_pos_brut[1:7]
  
  # Loop on values
  for(i in 8:nrow(tmp)){
    tmp[i, "nb_C0_dl"] <- tmp[i, "nb_C0"] - tmp[i-1, "nb_C0"] + tmp[i-7, "nb_C0_dl"]
    tmp[i, "nb_C1_dl"] <- tmp[i, "nb_C1"] - tmp[i-1, "nb_C1"] + tmp[i-7, "nb_C1_dl"]
    tmp[i, "nb_pos_dl"] <- tmp[i, "nb_pos"] - tmp[i-1, "nb_pos"] + tmp[i-7, "nb_pos_dl"]
  }
  
  if(diagnosticPlot){
    plot(tmp$nb_C0_brut)
    points(tmp$nb_C0_dl, col = 2)
    
    plot(tmp$nb_C1_brut)
    points(tmp$nb_C1_dl, col = 2)
    
    plot(tmp$nb_pos_brut)
    points(tmp$nb_pos_dl, col = 2)
  }
  
  # Output
  tmp
}

delissage("Île-de-France")
