# Script to plot "predictions" on Omicron spread, to alert

# NB: this script uses "unaveraged" data computed from a few lines of raw data that I have been communicated. Public data are only available with 7-day averages

####### Main initializations #######

dlData <- TRUE # Whether to download the case data 
# (just put FALSE when evaluating multiple times on the same day)

layout(1)

####### 1) Proportion of Omicron cases #######

# Initializations ####

# Useful functions
mylogit <- function(p) log(p/(1-p))
myinvlogit <- function(x) exp(x) / (1 + exp(x))

regNB <- 11 # 11 is Île-de-France
thecol <- "#FD9100" # Main color of the points
denom <- 7 # Denominator for case data, to plot weekly averages instead of weekly sums

# Load data "delissees"
# (I was sent a few lines of raw data, and averaged data can therefore be "un-averaged")
tmp <- read.csv(paste0("../data/deliss_reg-", regNB, ".csv"))
tmp$date2 <- as.Date(tmp$date2)

regName <- unique(tmp$reg_name) # Full name of the region

# Create data frame with just the data we need
# Here we use RAW numbers of C0 and C1 (L452R criblage)
# ("dl" is for "délissées")
a <- data.frame(date2 = tmp$date2, nb_C0 = tmp$nb_C0_dl, nb_C1 = tmp$nb_C1_dl)

# Time
first_datapoint <- "2021-11-15" # Time point at which the plot starts
# Compute number of days since first day (for regression)
a$time <- as.numeric(as.Date(a$date2) - as.Date(first_datapoint)) 
# Subset the data to the times we want
suba <- a[a$date2 > first_datapoint,]

# Number of days for projecting data
dx <- 10

# Figure #####
par(mgp = c(2.5, 0.25, 0))
par(mar = c(5, 2, 4, 2))

# Initialize plot
plot(NULL, pch = 20, type = "o", las = 1, axes = F, xlab = "", ylab = "",
     xlim = c(min(suba$time), max(suba$time) + dx), ylim = c(0, 1), yaxs = "i", xaxs = "i")

# Title 
mtext(side = 3, line = 2, text = regName, font = 2)
mtext(side = 3, line = 1, text = "Proportion non-L452R (suspicions Omicron)")
#mtext(side = 3, line = 0, text = paste0("Ajustement sur les données depuis le ", format(as.Date(first_date), "%d/%m")), cex = 0.8)
#mtext(side = 3, text = paste0(regName, "\nProportion non-L452R (suspicions Omicron)\nAjustement sur les données depuis le ", format(as.Date(first_date), "%d/%m")), line = 0, adj = 0.5)

# Horizontal lines, graduations
colHz <- gray(0.8) # Color of the horiz lines
for(i in seq(0, 1, by = 0.1)) abline(h = i, col = colHz)
abline(h = 0.5, col = colHz, lwd = 3) # Wider line for 0.5

# X axis
X <- c(suba$time, max(suba$time) + 1:dx) # Positions
Xl <- format(seq(min(suba$date2), max(suba$date2) + dx, by = "day"), "%d/%m") # Labels
cex.date <- 0.6 # Sise of the text labels
axis(1, at = X, labels = Xl, las = 2, cex.axis = cex.date, lwd = 0)

# Y axis
axis(2, at = seq(0, 1, 0.2), las = 1, lwd = 0)

# Graduations of the 0.5 horizontal lines to show days
dyy <- 0.01 # Heights of the graduations
arrows(x0 = X, 
       x1 = X, 
       y0 = 0.5 - dyy, y1 = 0.5 + dyy, col = colHz, code = 0)

# Dates on the 0.5 axis
ii <- (length(X) - dx): length(X) # indices of the last days
par(xpd = TRUE)
text(x = X[ii], y = 0.5 + dyy, srt = 90, adj = 0, labels = Xl[ii], cex = cex.date)
par(xpd = FALSE)

# Estimation
#first_date <- "2021-12-05" 

# Function to compute and plot estimation
plotEstimate <- function(first_date, col, cex.fit){
  # first_date: date at which estimation starts
  # col: color for plotting
  # cex.fit: cex of the date points that are highlighted
  
  # Subset of the data after the first date
  sub <- (a$date2 >= first_date)
  # Time vector, numeric, for the regression
  time_vec <- as.numeric(a[sub, "time"]) 
  
  # Fit logistic model
  glm0 <- glm(as.matrix(a[sub, c("nb_C0", "nb_C1")]) ~  time_vec, family = binomial(link="logit"))
  # Print summary
  print(summary(glm0))
  glm0$coefficients
  # x values for the prediction (dx days after the last one)
  xpred <- seq(min(time_vec) - dx, max(time_vec) + dx)
  # Predicted values from the model
  pred <- predict(glm0, type = "link", newdata = data.frame(time_vec = xpred), se.fit = TRUE)
  
  # Plot the contour of the prediction as polygon +/- CI
  polygon(x = c(xpred, rev(xpred)), y = c(myinvlogit(pred$fit - 1.96 * pred$se.fit), rev(myinvlogit(pred$fit + 1.96 * pred$se.fit))), col = adjustcolor(col, 0.4), border = NA)
  
  # Highlight the points used for the fit
  #points(a[sub, "time"], a[sub, "nb_C0"]/(a[sub, "nb_C1"] + a[sub, "nb_C0"]), pch = 16, type = "p", col = col, cex = cex.fit)
  
  # Return data frame of the prediction
  data.frame(xpred, pred)
} # end plotEstimate function


# Some graphical parameters
# 1 and 2 are first and second choice of first date for the estimation
# Colors
col1 <- "#0000B1"
col2 <- "#00AE63"
# Number of days taken into account for the estimation
n1 <- 10
n2 <- 15
# cex of the highlighted points
cx1 <- 1.6
cx2 <- 1.3
# Compute estimations and plot
e1 <- plotEstimate(max(a$date2) - n1 + 1, col1, cex.fit = cx1)
e2 <- plotEstimate(max(a$date2) - n2 + 1, col2, cex.fit = cx2)

# Add the data points
par(xpd = TRUE)
# Proportion of C0
points(suba$time, suba$nb_C0/(suba$nb_C1+suba$nb_C0), pch = 16, type = "p", col = thecol, cex = 1.1)
par(xpd = FALSE)

# CI
pp <- suba$nb_C0/(suba$nb_C1+suba$nb_C0)
nn <- (suba$nb_C1+suba$nb_C0)
dci <- 1.96 * sqrt(pp * (1-pp)/nn)

arrows(x0 = suba$time, 
       x1 = suba$time, 
       y0 = pp - dci, 
       y1 = pp + dci, 
       col = adjustcolor(thecol, 0.7), 
       lwd = 5, lend = 1, 
       code = 0)

# Legend
legend("topleft", box.lwd = 0, 
       col = c(col1, col2), pch = 15, 
       pt.cex = 1, #c(cx1, cx2),
       legend = c(paste(n1, "derniers jours"), paste(n2, "derniers jours")), 
       title = "Ajustement sur les données des", cex = 0.8)
#points(xpred, myinvlogit(pred$fit), type = "l")

# Credits
mtext(side = 1, paste0("@flodebarre et @FrancoisJB, mise à jour ", Sys.Date(), "
Données (sont ensuite 'délissées'):
https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/
Code: https://github.com/flodebarre/nouveauCriblage/blob/main/scripts/projections.R"), cex = 0.5, family = "mono", line = 3.5, adj = 0, col = gray(0.3))






####### 2) Projection of the number of cases #######

### Number of cases

# Mid point of the date interval (date2 is the end date)
tmp$dateMid <- as.Date(tmp$date2)-3
# Compute time since the first chosen data point
tmp$time <- as.numeric(as.Date(tmp$dateMid) - as.Date(first_datapoint))

# Fraction of Omicron
# Raw
tmp$pdl <- tmp$nb_C0_dl / (tmp$nb_C0_dl + tmp$nb_C1_dl)
# From averaged data (public data)
tmp$p7j <- tmp$nb_C0 / (tmp$nb_C0 + tmp$nb_C1)

# Load case data
# Source: https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#
URL <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"
if(dlData){
  download.file(URL, "../data/casesReg.csv")
}
datCases <- read.csv("../data/casesReg.csv", sep = ";")

# Subset the data to all ages, and to the chosen region
dC <- datCases[which(datCases$cl_age90 == 0 & datCases$reg == regNB), ]# & datCases$jour >= first_datapoint), ]
# Compute time since first_datapoint
dC$time <- as.numeric(as.Date(dC$jour) - as.Date(first_datapoint))


# Weekly averaged case data
#plot(tmp$time - 3, tmp$nb_pos/denom, ylim = c(0, 1.1*max(tmp$nb_pos/denom)))
#axis(1, at = X, labels = Xl, las = 2, cex.axis = cex.date, lwd = 0)

#points(dC$time, dC$P)

# Compute 7-day average of positive tests data
dC$P7j <- sliding.window(dC$P)
#points(dC$time, dC$P7j, col = "blue")
dC$jour <- as.Date(dC$jour)

# Merge datasets: add case data to the variant dataset
# common day: midpoint value
tmpM <- merge(tmp, dC, by.x = "dateMid", by.y = "jour", all = TRUE)
#tmpM <- tmpM[which(tmpM$dateMid > first_datapoint), ]
head(tmp)
names(dC)


#-----------------------------------------------------
# Source Plot
#source("JBMplot.R")
