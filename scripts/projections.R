mylogit <- function(p) log(p/(1-p))
myinvlogit <- function(x) exp(x) / (1 + exp(x))

regNB <- 11
thecol <- "#FD9100"

# Load data "delissees"
tmp <- read.csv(paste0("../data/deliss_reg-", regNB, ".csv"))
tmp$date2 <- as.Date(tmp$date2)

regName <- unique(tmp$reg_name)

a <- data.frame(date2 = tmp$date2, nb_C0 = tmp$nb_C0_dl, nb_C1 = tmp$nb_C1_dl)
#a <- read.csv("~/ownCloud/coronavirus/Omicron/extractionIDF.csv")
#View(a)
names(a)

plot(a$date2, a$nb_C0/(a$nb_C1+a$nb_C0), type = "l")


#first_date <- 185

first_datapoint <- "2021-11-15"
a$time <- as.numeric(as.Date(a$date2) - as.Date(first_datapoint))
suba <- a[a$date2 > first_datapoint,]

dx <- 10

# figure
par(mgp = c(2.5, 0.25, 0))

plot(NULL, pch = 20, type = "o", las = 1, axes = F, xlab = "", ylab = "",
     xlim = c(min(suba$time), max(suba$time) + dx), ylim = c(0, 1), yaxs = "i", xaxs = "i")
mtext(side = 3, line = 2, text = regName, font = 2)
mtext(side = 3, line = 1, text = "Proportion non-L452R (suspicions Omicron)")
#mtext(side = 3, line = 0, text = paste0("Ajustement sur les données depuis le ", format(as.Date(first_date), "%d/%m")), cex = 0.8)
#mtext(side = 3, text = paste0(regName, "\nProportion non-L452R (suspicions Omicron)\nAjustement sur les données depuis le ", format(as.Date(first_date), "%d/%m")), line = 0, adj = 0.5)

# Horizontal lines
colHz <- gray(0.8)
for(i in seq(0, 1, by = 0.1)) abline(h = i, col = colHz)
abline(h = 0.5, col = colHz, lwd = 3) # Bigger line for 0.5

# X axis
X <- c(suba$time, max(suba$time) + 1:dx)
Xl <- format(seq(min(suba$date2), max(suba$date2) + dx, by = "day"), "%d/%m")
axis(1, at = X, labels = Xl, las = 2, cex.axis = 0.8, lwd = 0)
axis(2, at = seq(0, 1, 0.2), las = 1, lwd = 0)

# Graduations
dyy <- 0.01
arrows(x0 = X, 
       x1 = X, 
       y0 = 0.5 - dyy, y1 = 0.5 + dyy, col = colHz, code = 0)

# Dates on the 0.5 axis
ii <- (length(X) - dx): length(X)
par(xpd = TRUE)
text(x = X[ii], y = 0.5 + dyy, srt = 90, adj = 0, labels = Xl[ii], cex = 0.6)
par(xpd = FALSE)

# Estimation
first_date <- "2021-12-05"
plotEstimate <- function(first_date, col, cex.fit){
  sub <- (a$date2 >= first_date)
  time_vec <- as.numeric(a[sub, "time"]) # need to define a vector with simple  name for the sake of predicting the outcome
  
  
  glm0 <- glm(as.matrix(a[sub, c("nb_C0", "nb_C1")]) ~  time_vec, family = binomial(link="logit"))
  print(summary(glm0))
  glm0$coefficients
  xpred <- seq(min(time_vec) - dx, max(time_vec) + dx)
  pred <- predict(glm0, type = "link", newdata = data.frame(time_vec = xpred), se.fit = TRUE)
  polygon(x = c(xpred, rev(xpred)), y = c(myinvlogit(pred$fit - 1.96 * pred$se.fit), rev(myinvlogit(pred$fit + 1.96 * pred$se.fit))), col = adjustcolor(col, 0.4), border = NA)
  
  # Points used for the fit
  points(a[sub, "time"], a[sub, "nb_C0"]/(a[sub, "nb_C1"] + a[sub, "nb_C0"]), pch = 16, type = "p", col = col, cex = cex.fit)
}

col1 <- "#0000B1"
col2 <- "#00AE63"
n1 <- 5
n2 <- 10
cx1 <- 1.6
cx2 <- 1.3
plotEstimate(max(a$date2) - n1 + 1, col1, cex.fit = cx1)
plotEstimate(max(a$date2) - n2 + 1, col2, cex.fit = cx2)
#plotEstimate("2021-12-07", "blue")

# Data points
par(xpd = TRUE)
points(suba$time, suba$nb_C0/(suba$nb_C1+suba$nb_C0), pch = 16, type = "p", col = thecol, cex = 0.8)
par(xpd = FALSE)

legend("topleft", box.lwd = 0, 
       col = c(col1, col2), pch = 16, 
       pt.cex = c(cx1, cx2),
       legend = c(paste(n1, "derniers jours"), paste(n2, "derniers jours")), 
       title = "Ajustement sur les données des", cex = 0.8)
#points(xpred, myinvlogit(pred$fit), type = "l")

mtext(side = 1, "@flodebarre et @FrancoisJB
Données :
https://www.data.gouv.fr/fr/datasets/
donnees-de-laboratoires-pour-le-
depistage-indicateurs-sur-les-mutations/
+ délissage

Code: https://github.com/flodebarre/
nouveauCriblage/blob/main/scripts/mutations.Rmd
", adj = 0, cex = 0.8, family = "mono")
