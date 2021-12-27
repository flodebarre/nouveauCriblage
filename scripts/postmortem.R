# Aim: check qualitative projections done on Dec 19
# This script is self-contained 
# You need to have folders `../pics/` and `../data/` for this to compile properly


# INITIALIZATIONS ######

rm(list = ls()) # I don't care what you think

# Parameters ####

# File name for saving
fname <- paste0("../pics/figPostMortem-IDF_", Sys.Date(), ".png")
# Whether to save as .png
plotPNG <- TRUE

# Whether to download data
# TRUE for a first evaluation
# FALSE if you are evaluating the file multiple times, to avoid downloading each time
dlData <- FALSE

# Threshold to consider criblage data reliable (as fraction of nb_pos)
thrp <- 0.05 

# Date range in the figure
minDate <- "2021-11-01"
maxDate <- "2021-12-21" 

# Colors
colOmicron <- "#FF4E2D"
colDelta <- gray(0.7)
colTot <- "black"
colIndet <- "#FF992D"

# Width of the bars
lwdd <- 15

# Other ####

# Function for sliding window average
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

# Open figure file
if(plotPNG){
  png(fname, width = 1000, height = 900, pointsize = 25)
}


# LOAD DATA ####

# Load criblage data
reg <- 11 # IDF INSEE CODE

URL_Regions <- "https://www.data.gouv.fr/fr/datasets/r/5ff0cad6-f150-47ea-a4e0-57e354c1b2a4"
if(dlData){
  download.file(URL_Regions, 
                destfile="../data/muts_Regions.csv",
                method="curl",
                extra='-L')
}
dat.Regions <- read.csv("../data/muts_Regions.csv", sep = ";", stringsAsFactors = FALSE)
# Format date
dat.Regions$date1 <- as.Date(substring(dat.Regions$semaine, 1, 10))
dat.Regions$date2 <- as.Date(substring(dat.Regions$semaine, 12, 21))
# Mid-interval date
dat.Regions$dateMid <- dat.Regions$date2 - 3

# Subset with the dates we want
datreg <- dat.Regions[which(dat.Regions$dateMid >= as.Date(minDate) & dat.Regions$dateMid <= as.Date(maxDate) & dat.Regions$reg == reg), ]
# Compute proportion cribles in criblage data
datreg$propCrib <- (datreg$nb_C0 + datreg$nb_C1) /datreg$nb_pos
# Compute proportion non-L452R
datreg$p <- (datreg$nb_C0) / (datreg$nb_C0 + datreg$nb_C1) 

# Load case data
# Source: https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#
URL <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"
if(dlData){
  download.file(URL, "../data/casesReg.csv")
}
datCases <- read.csv("../data/casesReg.csv", sep = ";")
# Subset with the dates we want
# (-3 and +3 with dates, because we download daily data and will compute sliding average
# from it; we loose 3 days on each side when doing so.)
# NB: 0 is the "all ages" age class
datCases <- datCases[which(as.Date(datCases$jour) >= (as.Date(minDate) - 3) & as.Date(datCases$jour) <= (as.Date(maxDate) + 3) & datCases$cl_age90 == 0 & datCases$reg == reg), ]
datCases$date <- as.Date(datCases$jour) # Use date format

# Check that dates are consecutive
stopifnot(all(diff(as.Date(datCases$jour)) == 1))
stopifnot(all(diff(as.Date(datreg$date2)) == 1))

# Compute sliding window average of case data
datCases$P7j <- sliding.window(datCases$P)

# Merge the datasets
tmp <- merge(datCases, datreg, by.x = "date", by.y = "dateMid", all = TRUE)


# PLOT ####

# Colors
#  Omicron
colsO <- rep(colOmicron, nrow(tmp))
colsO[tmp$propCrib <= thrp] <- colIndet # Different color if not enough criblage
#  Delta
colsD <- rep(colDelta, nrow(tmp))
colsD[tmp$propCrib <= thrp] <- colIndet # Different color if not enough criblage

# Start plot
par(las = 1, xpd = TRUE, mgp = c(2, 0, 0))
plot(as.Date(tmp$date), tmp$P7j, 
     type = "h", lwd = lwdd, col = colsO, lend = 1, 
     ylim = c(0, 25000), xlim = c(as.Date(minDate), as.Date(maxDate)),
     axes = FALSE, 
     xlab = "", ylab = "")
xD <- seq(as.Date(minDate), as.Date(maxDate), by = "day")
axis(1, at = xD, labels = format(xD, "%d/%m  "), las = 2, cex.axis = 0.7, lwd = 0, pos = 0)
axis(2, lwd = 0)

# Add non-Omicron cases
points(as.Date(tmp$date), (1 - tmp$p) * tmp$P7j, 
       type = "h", lwd = lwdd, col = colsD, lend = 1)

# Add cas bruts
# (uncomment to add)
# points(as.Date(datCases$jour), datCases$P, col = colTot, pch = 20)

# Legend
# Uncomment to add legend cas bruts
legend("topleft", 
       col = c(colDelta, colOmicron, colIndet, colTot), 
       pch = c(15, 15, 15#, 20
               ), 
       lwd = c(0, 0, 0#, 0
               ), 
       legend = c("Avec L452R (cas moyenne 7j)", "Sans L452R (cas moyenne 7j)", paste0("Plus assez de criblage (<", thrp * 100, "% des tests positifs)")#, "Cas totaux bruts"
                  ), 
       bty = "n")

par(xpd = FALSE)
# Line for projection (to look like the previous figure)
abline(v = as.Date("2021-12-13") + 0.5, lty = 2, lwd = 5, col = "#21809F")

# Credits
mtext(side = 1, paste0("", Sys.Date(), "
Données variants: https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/
Données cas : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#
Code: https://github.com/flodebarre/nouveauCriblage/blob/main/scripts/postmortem.R"), cex = 0.45, family = "mono", line = 3.5, adj = 0, col = gray(0.3))

title("Île-de-France")

# Close device, if necessary
if(plotPNG){
  dev.off()
  # Open the file
  system(paste0("open ", fname)) 
}






