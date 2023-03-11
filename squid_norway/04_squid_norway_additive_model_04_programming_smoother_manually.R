setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Programming a smoother manually
#   - as a final test to confirm whether a linear relationship is more suitable for these data.
# ------------------------------------------------------------------------------
# scaling ML
rg <- range(Squid$ML)
Squid$MLsc <- (Squid$ML - rg[1]) / (rg[2] - rg[1])


# We use 3 inner knots with positions determined by the quantiles of the scaled ML.
probs <- seq(0, 1, length = 5)
QD    <- quantile(unique(Squid$MLsc), probs)
QD


# functions for a natural cubic spline
rhs <- function(x, TH) {ifelse(x >= TH, (x-TH)^3,0)}
dk  <- function(x,TH,K){ (rhs(x,TH) - rhs(x,K)) / (K-TH) }
bj  <- function(x,TH,K){ dk(x,TH,K) - dk(x,K-1,K)}


# to plot smoother, it is easier to order the data according to the values of mantle length.
I1     <- order(Squid$MLsc)
Squid1 <- Squid[I1,]


# ----------
# linear regression: the basis for the natural cubic spline already contains an intercept.
M4 <- lm(d15N ~ 1 + Lat + MLsc  + bj(MLsc, QD[2], QD[4]) + bj(MLsc, QD[3], QD[4]) , data = Squid1)

X <- model.matrix(M4)
head(X)



# ----------
coef(M4)
Smooth <- X[,3:5] %*% coef(M4)[3:5]
Smooth <- Smooth - mean(Smooth)



# ----------
par(mfrow=c(1,1))
plot(x = Squid1$MLsc, y = Squid1$d15N, xlab = "Scaled ML", type = "n", ylab = "Smoothing function ML", ylim = c(-2.5,2.5), cex = 0.7,  pch = 16,  col = grey(0.5))

E4 <- resid(M4)
lines(Squid1$MLsc, Smooth, lwd = 5)
for (i in 1:5){abline(v = QD[i])}
points(Squid1$MLsc, Smooth+E4)



# ----------
# Compare the model with lm results
M5 <- lm(d15N ~ Lat + MLsc , data = Squid1)
anova(M4, M5, test = "F")


# -->
# additive model is superior to the linear regression model.


