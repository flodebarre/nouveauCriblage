# Script to plot a figure similar to John Burn-Murdoch's striking plot
# https://twitter.com/jburnmurdoch/status/1469427329906663424?s=20

# File name for saving
fname <- paste0("../pics/figJBM-IDF_", Sys.Date(), ".png")
# Whether to save as .png
plotPNG <- TRUE

if(plotPNG){
  png(fname, width = 1000, height = 800, pointsize = 25)
}

dx <- 9 # Number of days of projections

# Ranges of the plot
xmax <- max(tmpM$time.x, na.rm = TRUE) + dx
xmin <- min(tmpM$time.x, na.rm = TRUE)   
ymax <- 2.5*max(tmpM$P7j, na.rm = TRUE)

# Colors
colOmicron <- "#FF4E2D"
colOmicronPoints <- "#B61C00"
colDelta <- gray(0.7)
colTot <- "black"

par(mgp = c(2.5, 0.5, 0))
par(mar = c(5, 1, 3.5, 1))

# Initialize plot
plot(tmpM$time.x, tmpM$P7j, 
     ylim = c(0, ymax), 
     xlim = c(xmin, xmax), 
     yaxs = "i", 
     axes = FALSE, 
     xlab = "", ylab = "", 
     type = "n")

# Index of the last non NA point
i.nna <- max(which(!is.na(tmpM$P7j)))
# x position of the last non NA point
x.nna <- tmpM$time.x[i.nna]

# Exponential fit to the -L452R data, since time 21
sub <- (tmpM$time.x >= 21)
time_vec <- as.numeric(tmpM[which(sub), "time.x"])
logP <- log(tmpM$P7j[which(sub)] * tmpM$pdl[which(sub)])
glmE <- glm(logP ~  time_vec)
# Predicted values
xpred <- seq(xmin, max(time_vec) + dx)
predLog <- predict(glmE, newdata = data.frame(time_vec = xpred))
# Plot prediction
#points(xpred, exp(predLog), col = 2)

# non Omicron cases 
nD <- rep(NA, xmax - xmin + 1)
nD[1:i.nna] <- tmpM[1:i.nna, "P7j"]

#nD[(i.nna + 1):length(nD)] <- nD[i.nna] + 
#points(tmpM[1:i.nna, "time.x"], nD[1:i.nna], col = 2))

iPred <- which(xpred > x.nna)
# Just check we have the right indices
# points(xpred[iPred], exp(predLog[iPred]), col = 3)

#points(xpred[iPred], tmpM[i.nna, "P7j"] * (1-tmpM$pdl[i.nna])  + exp(predLog[iPred]), col = 3)



tmpM$time.x

date0 <- tmpM[which(tmpM$time.x == 0), "dateMid"]


ii <- which(is.element(tmpM$dateMid, as.Date(c("2021-11-01", "2021-11-15", "2021-12-01", "2021-12-15", "2021-12-25"))))
ii

# Add time graduations
#axis(1, at = seq(xmin, xmax), labels = rep("", xmax - xmin + 1), tck = -0.01)
axis(1, at = tmpM[ii, "time.x"], labels = format(tmpM[ii, "dateMid"], "%d %b"), adj = 0.5, tck = -0.02)

axis(4, lwd = 0, las = 1, cex.axis = 0.65, pos = xmax)

par(xpd = TRUE)
# Shading for Omicron
polygon(x = c(xpred, rev(xpred)), 
        y = c(exp(predLog), rep(0, length(xpred))), 
        col = colOmicron, border = colOmicron)

# Shading for Delta
polygon(x = c(xpred, rev(xpred)), 
        y = c(nD[which(tmpM$time.x == xmin):length(nD)], tmpM[i.nna, "P7j"] * (1-tmpM$pdl[i.nna])  + exp(predLog[iPred]), rev(exp(predLog))), 
        col = colDelta, border = colDelta)

# Shading for projection
yM <- 2*max(exp(predLog))
polygon(x = c(x.nna + 0.5, x.nna + 0.5, xmax, xmax), 
        y = c(0, yM, yM, 0), 
        col = gray(1, alpha = 0.3), 
        border = NA)

par(xpd = FALSE)

abline(v = x.nna + 0.5, lty = 2, lwd = 3)

# -L452R cases
points(tmpM$time.x, tmpM$pdl * tmpM$P7j, col = colOmicronPoints, pch = 20)

# Total cases
#points(tmpM$time.x, tmpM$P7j, pch = 20, col = colTot)

par(xpd = TRUE)
text(x = mean(c(x.nna, xmax)), y = ymax, 
     adj = c(0.5, -1), labels = "Projection", cex = 1.2)

title("Île-de-France")

# Add labels for the variants
yll <- 3750
cexl1 <- 1.2
cexl2 <- 0.8
adj2 <- 1.5
ft1 <- 2

text(15, yll, "Delta", adj = c(0.5, 0), cex = cexl1, font = ft1)
text(15, yll, "et autres variants \navec L452R", adj = c(0.5, adj2), cex = cexl2)

dxxl <- 1.5
text(mean(c(x.nna, xmax)) + dxxl, yll, "Omicron", col = colOmicronPoints, adj = c(0.5, 0), cex = cexl1, font = ft1)
text(mean(c(x.nna, xmax)) + dxxl, yll, "et autres variants \nsans L452R", col = colOmicronPoints, adj = c(0.5, adj2), cex = cexl2)

yyl <- 0.85*ymax
text(x = xmin, y = yyl, adj = c(0, 0), 
     labels = "Projections à l'aide du criblage sur L452R : 
- Delta a la mutation L452R, 
")

text(x = xmin, y = yyl, adj = c(0, 0), 
     labels = "
     
- Omicron ne l'a pas*", col = colOmicronPoints)


text(x = xmin, y = yyl, adj = c(0, 2), 
     labels = "* B.1.640 ne l'a pas non plus, mais la part d'Omicron devient vite largement supérieure
  Quelques Omicron (de l'ordre de 1% dans GISAID) ont L452R", 
     cex = 0.55, col = gray(0.2))

par(xpd = FALSE)


# Credits
mtext(side = 1, paste0("Mise à jour ", Sys.Date(), ". Figure inspirée de @jburnmurdoch, FT
Données variants (sont ensuite 'délissées'):
https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/
Données cas : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#
Code: https://github.com/flodebarre/nouveauCriblage/blob/main/scripts/projections.R"), cex = 0.5, family = "mono", line = 3.5, adj = 0, col = gray(0.3))

# Close device, if necessary
if(plotPNG){
  dev.off()
  system(paste0("open ", fname)) 
}
