mylogit <- function(p) log(p/(1-p))
myinvlogit <- function(x) exp(x) / (1 + exp(x))


a <- data.frame(date2 = tmp$date2, nb_C0 = tmp$nb_C0_dl, nb_C1 = tmp$nb_C1_dl)

#a <- read.csv("~/ownCloud/coronavirus/Omicron/extractionIDF.csv")
#View(a)
names(a)
plot(a$date2, a$nb_C0/(a$nb_C1+a$nb_C0), type = "l")





first_date <- 185
a$time <- as.numeric(as.Date(a$date2) - as.Date(first_date))

suba <- a[a$date2 > "2021-11-01",]

first_date <- "2021-12-05"
sub <- (a$date2 >= first_date)
time_vec <- as.numeric(as.Date(a$date2[sub]) - as.Date(first_date)) # need to define a vector with simple  name for the sake of predicting the outcome


glm0 <- glm(as.matrix(a[sub, c("nb_C0", "nb_C1")]) ~  time_vec, family = binomial(link="logit"))
summary(glm0)
glm0$coefficients
#xpred <- seq(186, 220)
xpred <- seq(-10, max(time_vec) + 10)
pred <- predict(glm0, type = "link", newdata = data.frame(time_vec = xpred), se.fit = TRUE)

# figure
plot(NULL, pch = 20, type = "o", las = 1, axes = F, xlab = "Time", ylab = "Frequency suspicions Omicron",
     xlim = range(xpred), ylim = c(0, 1))
axis(1, at = xpred, labels = format(seq(as.Date(first_date) + min(xpred), as.Date(first_date) + max(xpred), by = "day"), "%d/%m"), las = 2)
axis(2, at = seq(0, 1, 0.2), las = 1)
polygon(x = c(xpred, rev(xpred)), y = c(myinvlogit(pred$fit - 1.96 * pred$se.fit), rev(myinvlogit(pred$fit + 1.96 * pred$se.fit))), col = "gray", border = NA)
points(suba$time, suba$nb_C0/(suba$nb_C1+suba$nb_C0), pch = 20, type = "p")
points(xpred, myinvlogit(pred$fit), type = "l")
abline(h = 0.5, lty = 2)
