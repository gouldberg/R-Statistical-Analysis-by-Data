# ------------------------------------------------------------------------------
# Data:  SharksGBRPM
# 
# Underlying task. 
# Model shark abundances as a function of: 
#   -zoning (e.g. areas closed / open to fishing). 
#   -number of days since the GBR re-zoning.
#   -habitat (percentage hard coral cover).
#   -reef proximity. 
#   -Sampling effort (hrs footage per site) as an offset?
#
# In the paper, several models were examined using:
#  1) all shark species combined; 
#  2) C. amblyrhynchos; 
#  3) C. albimarginatus; 
#  4) G. cuvier 
#
# Species identity:
# GRS - C. amblyrhynchos   – grey reef shark
# STS - C. albimarginatus  – silver tip shark
# TGS – G. cuvier          – tiger shark  
# WTS – T. obesus          – whitetip reef shark - 
#
#
# The plan is:
# Sharks = function(Zoning, Days since re-zoning, 
#                   Percentage Hard Coral, distance reef, 
#                   soaking time)
#
#
# TAKEAWAY:
#  - If the dispersion is not too large then a NB GLM is not always the best solution
#  - A quasi-Poisson can be used in such cases but it should be noted that this modelling approach does not solve the source of the overdispersion and my result in biased parameter estimates
#  - Interpretation of a ZIP GLM can be difficult, you need to create graphs !!
#
# For modeling maximum number of silvertips sharks (STS), even if ZIP GLM has best AIC, the a lot of zero values are not fitted well
#  - Analyzing the other species may yield similar ecological conclusions, giving the illusion that there are major thins going in this dataset.
#   However, the species may interact with one another. This is pseudo-replication !
#  - Option1:  Split dataset into different parts and analyse one specific species for each part. --> dataset is not so large enough
#  - Option2:  Apply multivariate generalized linear mixed model, adding dependency structure between shar species measured at the same site
#  - Option3:  Apply multivariate normal priors, some species may need a Poisson GLM, whereas others need a NB GLM or ZIP GLM
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS", "VGAM", "broom")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurZero-InflatedModelswithR/SharksGBRMP.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

# SoakTime is sampling effort
# we use SoakTime transformed by natural log:  due to the exponential relationship between the expected number of TGS and the covariates
data$LogSoakTime <- log(data$SoakTime)


# ------------------------------------------------------------------------------
# multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------
Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}

MyVar <- c("TGS", "DistReef", "HardCoral", "LogSoakTime")
Mydotplot(data[, MyVar])


# ------------------------------------------------------------------------------
# Reponse variable (TGS) versus continuous covariates
#
# TGS:  the maximum number of observed tiger sharks counts
# ------------------------------------------------------------------------------
MyData <- data.frame(Y = rep(data$TGS, 3), X = c(data$DistReef, data$HardCoral, data$SoakTime),
                     ID = rep(c("Distance to reef", "Hard coral percentage", "Soak time"), each = nrow(data)))

p1 <- ggplot()
p1 <- p1 + xlab("Covariate") + ylab("TGS counts")
p1 <- p1 + theme(text = element_text(size=15))
p1 <- p1 + geom_point(data = MyData, aes(x = X, y = Y))
p1 <- p1 + facet_grid(. ~ ID, scales = "free")
p1


MyVar <- c("TGS", "DistReef", "HardCoral", "LogSoakTime")
# MyVar <- c("TGS", "DistReef", "HardCoral", "SoakTime")
psych::pairs.panels(data[, MyVar ])


# ------------------------------------------------------------------------------
# Check zero counts of tiger sharks
# ------------------------------------------------------------------------------

sum(data$TGS == 0) / nrow(data)


# ------------------------------------------------------------------------------
# Poisson GLM to model counts of tiger sharks
# ------------------------------------------------------------------------------
M1 <- glm(TGS ~ DistReef + HardCoral + Zoning + LogSoakTime, data = data, family = poisson)
summary(M1)


# generalized R^2  --> 50% is adequate
( M1$null.deviance - M1$deviance ) / M1$null.deviance


# use "SoakTime" instead of "LogSoakTime"
M1a <- glm(TGS ~ DistReef + HardCoral + Zoning + SoakTime, data = data, family = poisson)
summary(M1a)
AIC(M1, M1a)


# the 95% confidence interval for the regression parameter of LogSoakTime does not contain 1, justifying the decision not to use this variable as an offset
confint(M1)


# Dispersion statistic
E1 <- resid(M1, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M1))
sum(E1^2) / (N - p)


# ------------------------------------------------------------------------------
# Try variable selection
# ------------------------------------------------------------------------------
drop1(M1, test = "Chi")

step(M1)

# delete "HardCoral"
M2 <- glm(TGS ~ DistReef + Zoning + LogSoakTime, data = data, family = poisson)
summary(M2)

confint(M2)
AIC(M2, M1)


# Dispersion statistic --> 1.34:  grey zone...it is just ok, especially because the two covariates that are significant are highly significant.
E2 <- resid(M1, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M2))
sum(E2^2) / (N - p)


# ------------------------------------------------------------------------------
# Poisson GLM:  Simulate dispersion statistic
# ------------------------------------------------------------------------------
MyDispersion <- function(y, mu, N, P){
  e <- (y - mu) / sqrt(mu)
  sum(e^2) / (N - P)
}

muFit <- fitted(M2)
N <- nrow(data)
P <- length(coef(M2))
sim_n <- 100000
Ysim <- matrix(nrow = N, ncol = sim_n)
DispersionSim <- vector(length = sim_n)
for(i in 1:sim_n){
  Ysim[,i] <- rpois(N, lambda = muFit)
  DispersionSim[i] <- MyDispersion(Ysim[,i], muFit, N, P)
}

hist(DispersionSim, breaks = seq(-1,5,0.05))
abline(v = sum(E2^2) / (N - p), col = 2, lty = 2)


# Results indicate that 5.3% of the simulated statistics are larger than the one we found for the model applied to the original data.
# It is grey area...
Dispersion <- sum(E2^2) / (N - p)
sum(DispersionSim > Dispersion) / sim_n


# ------------------------------------------------------------------------------
# Poisson GLM:  Simulate zero counts ratio
#
# !!! NOTE that a large number of zeros does not mean that zero-inflated models must be applied.
# In this case, covariates in the Poisson GLM can explain the excessive number of tiger sharks zeros !!! (89%)
# ------------------------------------------------------------------------------
sum(data$TGS == 0) / N
sum(Ysim[,1] == 0) / N
sum(Ysim[,2] == 0) / N

zeros <- vector(length = sim_n)
for (i in 1:sim_n){
  zeros[i] <- sum(Ysim[,i] == 0) / N
}

par(mar = c(5,5,2,2))
plot(table(zeros),  xlim = c(0.80, 0.94), axes = FALSE, xlab = "Percentage of zeros", ylab = "Frequency", cex.lab = 1.5)
axis(2)
axis(1, at = c(0.80, 0.82, 0.84, 0.86, 0.88, 0.90, 0.92, 0.94), labels = c("80%", "82%", "84%", "86%", "88%", "90%", "92%", "94%"))
points(x = sum(data$TGS==0) / N, y = 0, pch = 16, cex = 5, col = 2)


# ------------------------------------------------------------------------------
# Posisson GLM:  Model Validation
# It seems that we do not detect any problem
# ------------------------------------------------------------------------------
par(mfrow = c(2,2), mar = c(5,5,2,2))
E2 <- resid(M2, type = "pearson")
F2 <- fitted(M2)

plot(x = F2, y = E2, xlab = "Fitted values", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

plot(x = data$HardCoral,y = E2, xlab = "HardCoral", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

plot(x = data$DistReef, y = E2, xlab = "DistReef", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

boxplot(E2 ~ Zoning,  xlab = "Zoning",  cex.lab = 1.5, data = data, ylab = "Residuals")
abline(h = 0, lty = 2)


# ------------------------------------------------------------------------------
# Poisson GLM:  Visualize the results in 3D (RGL)
# ------------------------------------------------------------------------------
set.seed(12345)
data$Bd.jitter  <- jitter(data$DistReef, amount = 0.1)
data$Prc.jitter <- jitter(data$LogSoakTime, amount = 0.1)

MyData <- expand.grid(DistReef = seq(0, 45, length = 25),
                      LogSoakTime  = seq(1.99, 8.5, length = 25),
                      Zoning    = levels(data$Zoning)
)

X    <- model.matrix(~ 1 + DistReef + Zoning + LogSoakTime, data = MyData)
eta  <- X %*% coef(M2)
mu   <- exp(eta)
ExpY <- mu  #Expected values Y. For a ZIP these will change. 
ExpY %>% head(10)

# And add the expected values to MyData
I1 <- MyData$Zoning == "Pre_zoning"
I2 <- MyData$Zoning == "Post_zoning"

MyData2 <- cbind(MyData[I1,], ExpY[I1])
MyData3 <- cbind(MyData[I2,], ExpY[I2])

BD.25   <- seq(0, 45, length = 25)
Prc.25  <- seq(1.99, 8.5, length = 25)

# And we convert the vector with expected values into
# a 25 by 25 matrix
ExpY1 <- ExpY[I1]
ExpY2 <- ExpY[I2]

# ExpY[ExpY > 10] <- NA
ExpY1.2d <- matrix(ExpY1, nrow = length(Prc.25), ncol = length(BD.25))
ExpY2.2d <- matrix(ExpY2, nrow = length(Prc.25), ncol = length(BD.25))

# Now plot the jittered data again
library(rgl)
plot3d(x = data$Bd.jitter, y = data$Prc.jitter, z = data$TGS, type = "p", size = 10, lit = FALSE, xlab = "DistReef", ylab = "log Soak time", zlab = "TGS counts", zlim = c(0,10)
       #col = MyColour
)

# Add the surface for the fitted Poisson values, one plane per zoning
surface3d(BD.25, Prc.25, ExpY1.2d, alpha = 0.6, front = "lines",  back = "lines",  color = "black", zlim = c(0,15))
surface3d(BD.25, Prc.25, ExpY2.2d, alpha = 0.6, front = "lines",  back = "lines",  color = "red", zlim = c(0,15))


# ------------------------------------------------------------------------------
# Poisson GLM:  model total number of all sharks species
# ------------------------------------------------------------------------------
data$TA       <- rowSums(data[,1:23])

MyData <- data.frame(Y = rep(data$TA, 3), X = c(data$DistReef, data$HardCoral, data$SoakTime),
                     ID = rep(c("Distance to reef", "Hard coral percentage", "Soak time"), each = nrow(data)))

p1 <- ggplot()
p1 <- p1 + xlab("Covariate") + ylab("TA counts")
p1 <- p1 + theme(text = element_text(size=15))
p1 <- p1 + geom_point(data = MyData, aes(x = X, y = Y))
p1 <- p1 + facet_grid(. ~ ID, scales = "free")
p1

MyVar <- c("TA", "DistReef", "HardCoral", "LogSoakTime")
Mydotplot(data[, MyVar])

M2 <- glm(TA ~ DistReef + HardCoral + Zoning + LogSoakTime, data = data, family = poisson)
summary(M2)
confint(M2)

# All covariates remain
drop1(M2, test="Chi")
step(M2)

# generalized R^2  --> 59% is adequate
( M2$null.deviance - M2$deviance ) / M2$null.deviance

# dispersion statistics:  2.42, which is too high to ignore.
E2 <- resid(M2, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M2))
sum(E2^2) / (N - p)

# zero counts:  52.8%
sum(data$TA == 0) / nrow(data)


# ------------------------------------------------------------------------------
# Poisson GLM:  model total number of all sharks species
# simulate dispersion statistics and zero counts percentage
#
# It seems that overdispersion may be due to zero inflation !!!
# ------------------------------------------------------------------------------
MyDispersion <- function(y, mu, N, P){
  e <- (y - mu) / sqrt(mu)
  sum(e^2) / (N - P)
}

muFit <- fitted(M2)
N <- nrow(data)
P <- length(coef(M2))
sim_n <- 100000
Ysim <- matrix(nrow = N, ncol = sim_n)
DispersionSim <- vector(length = sim_n)
for(i in 1:sim_n){
  Ysim[,i] <- rpois(N, lambda = muFit)
  DispersionSim[i] <- MyDispersion(Ysim[,i], muFit, N, P)
}

par(mfrow=c(1,1))
hist(DispersionSim, breaks = seq(-1,5,0.05))
abline(v = sum(E2^2) / (N - p), col = 2, lty = 2)

Dispersion <- sum(E2^2) / (N - p)
sum(DispersionSim > Dispersion) / sim_n

sum(data$TA == 0) / N
sum(Ysim[,1] == 0) / N
sum(Ysim[,2] == 0) / N

zeros <- vector(length = sim_n)
for (i in 1:sim_n){
  zeros[i] <- sum(Ysim[,i] == 0) / N
}

par(mar = c(5,5,2,2))
plot(table(zeros),  xlim = c(0.32, 0.52), axes = FALSE, xlab = "Percentage of zeros", ylab = "Frequency", cex.lab = 1.5)
axis(2)
axis(1, at = c(0.32, 0.36, 0.40, 0.44, 0.48, 0.52), labels = c("32%", "36%", "40%", "44%", "48%", "52%"))
points(x = sum(data$TA==0) / N, y = 0, pch = 16, cex = 5, col = 2)


# ------------------------------------------------------------------------------
# Apply negative binomial GLM:  model total number of all sharks species
# ------------------------------------------------------------------------------
library(MASS)
M3 <- glm.nb(TA ~ DistReef + HardCoral + Zoning + LogSoakTime, data = data)

# extra parameter "k" in the variance of the NB is estimated as 1.1344
# Do not confuse the dispersion statistic with the paramter k !!
summary(M3)

# overdispersion: 1.099583
# "+1" is due to the extra parameter k in the variance of the NB 
E3 <- resid(M3, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M3)) + 1
sum(E3^2) / (N - p)

# model validation
par(mfrow = c(2,2), mar = c(5,5,2,2))
E3 <- resid(M3, type = "pearson")
F3 <- fitted(M3)

plot(x = F3, y = E3, xlab = "Fitted values", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

plot(x = data$HardCoral, y = E3, xlab = "HardCoral", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

plot(x = data$DistReef, y = E3, xlab = "DistReef", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

boxplot(E3 ~ Zoning,  xlab = "Zoning",  cex.lab = 1.5, data = data, ylab = "Residuals")
abline(h = 0, lty = 2)


# ------------------------------------------------------------------------------
# Apply zero-inflated model (ZIP):  model total number of all sharks species
#
# !!! NOTE that a large number of zeros does not mean that zero-inflated models must be applied.
# ------------------------------------------------------------------------------
library(pscl)
M4 <- zeroinfl(TA ~ DistReef + HardCoral + Zoning + LogSoakTime | DistReef + HardCoral + Zoning + LogSoakTime, data = data)
summary(M4)

# overdispersion: 1.5569
E4 <- resid(M4, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M4))
sum(E4^2) / (N - p)


# Compare Poisson GLM, NB GLM, ZIP  --> NB GLM is clearly better than the others
AIC(M2, M3, M4)


# ------------------------------------------------------------------------------
# Model STS (silvertip sharks), which species has 92% zeros !!!
# Apply Poisson and quasi-Poisson GLM
# ------------------------------------------------------------------------------
MyData <- data.frame(Y = rep(data$STS, 3), X = c(data$DistReef, data$HardCoral, data$SoakTime),
                     ID = rep(c("Distance to reef", "Hard coral percentage", "Soak time"), each = nrow(data)))

p1 <- ggplot()
p1 <- p1 + xlab("Covariate") + ylab("STS counts")
p1 <- p1 + theme(text = element_text(size=15))
p1 <- p1 + geom_point(data = MyData, aes(x = X, y = Y))
p1 <- p1 + facet_grid(. ~ ID, scales = "free")
p1

MyVar <- c("STS", "DistReef", "HardCoral", "LogSoakTime")
Mydotplot(data[, MyVar])

# zero counts:  92.3% !!!!
sum(data$STS == 0) / nrow(data)

# Poisson GLM --> overdispersion statistic is 1.92
M5 <- glm(STS ~ 1 + DistReef + HardCoral + Zoning  + LogSoakTime, family = "poisson",  data = data)
summary(M5)          

E5 <- resid(M5, type = "pearson")
N  <- nrow(data)
p  <- length(coef(M5))
sum(E5^2) / (N - p)


# Quasi poisson GLM --> dispersion paramter 1.91
# Quasi Poisson is not a statistical distribution. It is a quick and dirty solution to deal with minor overdispersion by increasing the standard errors of the regression parameters
# with the sqare root of the dispersion statistic.
# It does not solve the original problem that is causing the overdispersion and this may result in biased parameter estimates
# AIC is not defined for this model
M5b <- glm(STS ~ 1 + HardCoral + Zoning  + LogSoakTime, family = "quasipoisson",  data = data)
summary(M5b)


# ------------------------------------------------------------------------------
# Model STS (silvertip sharks), which species has 92% zeros !!!
# Apply negative binomial GLM and ZIP model
# ------------------------------------------------------------------------------
M6 <- glm.nb(STS ~ 1 + DistReef + HardCoral + Zoning  + LogSoakTime, data = data)
M7 <- zeroinfl(STS ~ 1 + DistReef + HardCoral + Zoning  + LogSoakTime  | 1 + DistReef + HardCoral + Zoning + LogSoakTime, data = data)
AIC(M5, M6, M7)

# overdispersion statistic is 0.91
E7 <- resid(M7, type = "pearson")
N  <- nrow(data)
p7  <- length(coef(M7))
sum(E7^2) / (N - p7)


# Backwords selection to start...
M7b <- zeroinfl(STS ~ 1 + DistReef + HardCoral + Zoning  + LogSoakTime  | 1 + DistReef + HardCoral + Zoning + LogSoakTime, data = data)
summary(M7b)

# the final....
M8 <- zeroinfl(STS ~ 1 + DistReef  + LogSoakTime  | 1 + DistReef + HardCoral + Zoning, data = data)
summary(M8)          
confint(M8)

AIC(M5, M6, M7, M8)


# Model validation
F8 <- fitted(M8)
E8 <- resid(M8)

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F8, y = E8, xlab = "Fitted values", ylab = "Residuals", cex.lab = 1.5)
abline(h = 0, lty = 2)     

boxplot(E8 ~ Zoning,  xlab = "Zoning",  cex.lab = 1.5, data = data, ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = F8, y = data$STS)


# ------------------------------------------------------------------------------
# Model STS (silvertip sharks), which species has 92% zeros !!!
# Final ZIP model:  Visualize the results  BINARY PART by ZONING
# ------------------------------------------------------------------------------
range(data$DistReef)
range(data$HardCoral)
MyData <- expand.grid(DistReef  = seq(0, 45, length = 25),
                      HardCoral = seq(0, 63, length = 25),
                      Zoning    = levels(data$Zoning)
)

beta.b <- coef(M8, model = "zero") 
X.b    <- model.matrix(~ 1 + DistReef + HardCoral + Zoning, data = MyData)
eta.b  <- X.b %*% beta.b
MyData$Pi   <- exp(eta.b) / (1 + exp(eta.b))

I1 <- MyData$Zoning == "Pre_zoning"
I2 <- MyData$Zoning == "Post_zoning"

MyData2 <- MyData[I1,]
MyData3 <- MyData[I2,]

X.25  <- seq(0, 45, length = 25)
Y.25  <- seq(0, 63, length = 25)
ExpY1 <- ExpY[I1]
ExpY2 <- ExpY[I2]

#ExpY[ExpY > 10] <- NA
Pi1.2d <- matrix(MyData$Pi[I1], nrow = 25, ncol = 25)
Pi2.2d <- matrix(MyData$Pi[I2], nrow = 25, ncol = 25)

library(rgl)
par(mfrow = c(1,2))
plot3d(x = data$DistReef, y = data$HardCoral, z = as.numeric(data$STS > 0), type = "p", size = 10, lit = FALSE, xlab = "DistReef", ylab = "HardCoral", zlab = "Pi", zlim = c(0,1)
       #col = MyColour
)

# Add the surface for the binary part
surface3d(X.25, Y.25, Pi1.2d,  alpha = 0.6,  front = "lines",  back = "lines",  color = "purple", zlim = c(0,1))
surface3d(X.25, Y.25, Pi2.2d,  alpha = 0.6,  front = "lines",  back = "lines",  color = "red", zlim = c(0,1))


# ------------------------------------------------------------------------------
# Model STS (silvertip sharks), which species has 92% zeros !!!
# Final ZIP model:  Visualize the results  COUNT PART
#
# NOTE that a lot of zero values are not fitted well !!! even if AIC is best up to now.
#
# the expected values of the ZIP are obtained by multiplying the count part with 1 minus the binary part
# ------------------------------------------------------------------------------
range(data$DistReef)
range(data$LogSoakTime)
MyData <- expand.grid(DistReef    = seq(0, 45, length = 25),
                      LogSoakTime = seq(1.99, 8.5, length = 25)
)

beta.c <- coef(M8, model = "count") 
X.c    <- model.matrix(~ 1 + DistReef  + LogSoakTime, data = MyData)
eta.c  <- X.c %*% beta.c
MyData$mu <- exp(eta.c)

X.25  <- seq(0, 45, length = 25)
Y.25  <- seq(1.99, 8.5, length = 25)

mu.2d <- matrix(MyData$mu, nrow = 25, ncol = 25)
mu.2d[mu.2d > 25] <- NA

open3d()
plot3d(x = data$DistReef, y = data$LogSoakTime, z = data$STS, type = "p", size = 10, lit = FALSE, xlab = "DistReef", ylab = "LogSoakTime", zlab = "STS counts", zlim = c(0,25)
       #col = MyColour
)

# Add the surface for the count part
surface3d(X.25, Y.25, mu.2d,  alpha = 0.6, front = "lines",  back = "lines",  color = "black", zlim = c(0,25))


# ------------------------------------------------------------------------------
# Model STS (silvertip sharks), which species has 92% zeros !!!
# Final ZIP model:  Visualize the results  COUNT PART by LogSoakTime
# ------------------------------------------------------------------------------
MyData <- expand.grid(DistReef    = seq(0, 45, length = 25),
                      LogSoakTime = c(2, 4, 6, 8)
)

beta.c <- coef(M8, model = "count") 
X.c    <- model.matrix(~ 1 + DistReef  + LogSoakTime, data = MyData)
eta.c  <- X.c %*% beta.c
MyData$mu <- exp(eta.c)

par(mfrow=c(1,1))
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(0, 0,  xlim = c(9, 45), ylim = c(0, 25), xlab = "Distance to reef", ylab = "STS fitted values")
lines(MyData$DistReef[MyData$LogSoakTime == 2], MyData$mu[MyData$LogSoakTime == 2])
lines(MyData$DistReef[MyData$LogSoakTime == 4], MyData$mu[MyData$LogSoakTime == 4])
lines(MyData$DistReef[MyData$LogSoakTime == 6], MyData$mu[MyData$LogSoakTime == 6])
lines(MyData$DistReef[MyData$LogSoakTime == 8], MyData$mu[MyData$LogSoakTime == 8])

text(40, 1, "LogSoakTime = 2")           
text(40, 3.5, "LogSoakTime = 4")           
text(40, 8, "LogSoakTime = 6")           
text(25, 18, "LogSoakTime = 8")           



X.25  <- seq(0, 45, length = 25)
Y.25  <- seq(1.99, 8.5, length = 25)

mu.2d <- matrix(MyData$mu, nrow = 25, ncol = 25)
mu.2d[mu.2d > 25] <- NA

open3d()
plot3d(x = data$DistReef, y = data$LogSoakTime, z = data$STS, type = "p", size = 10, lit = FALSE, xlab = "DistReef", ylab = "LogSoakTime", zlab = "STS counts", zlim = c(0,25)
       #col = MyColour
)

# Add the surface for the count part
surface3d(X.25, Y.25, mu.2d,  alpha = 0.6,  front = "lines",  back = "lines",  color = "black", zlim = c(0,25))

