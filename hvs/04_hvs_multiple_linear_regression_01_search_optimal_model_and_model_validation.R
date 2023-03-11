setwd("//media//kswada//MyFiles//R//hvs")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HVS
# ------------------------------------------------------------------------------
HVS <- read.table(file = "HVS.txt", header = TRUE, dec=".")


str(HVS)

names(HVS)


# ----------
# The variable names are long, we will rename some of them
# We also define categorical variables as factors, to avoid mistakes.
HVS$OrbitalV    <- HVS$MeanOrbitalVolume
HVS$fPopulation <- factor(HVS$Population)
HVS$LatAbs      <- HVS$AbsoluteLatitude
HVS$CC          <- HVS$CranialCapacity
HVS$FM          <- HVS$FMarea_intercondyle
HVS$Illuminance <- HVS$Minimum_Illuminance
HVS$Temperature <- HVS$Minimum_Temperature_celsius
HVS$fGender     <- factor(HVS$Gender)



# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

# There is one missing value
colSums(is.na(HVS))


# we remove it
# The function na.exvlude removes all rows that contain a missing value.
HVS2 <- na.exclude(HVS)



# ------------------------------------------------------------------------------
# Multiple linear regression
# ------------------------------------------------------------------------------

# Fit the multiple linear regression model
M1 <- lm(OrbitalV ~ LatAbs + CC + FM + LatAbs : CC + LatAbs : FM + CC : FM, data = HVS2)


print(summary(M1), digits = 3, signif.stars = FALSE)



# -->
# Although we have not done it here, it is wise to centre each covariate (subtracting its mean),
# as this may avoid potential numerical problems due to collinearity.
# An interaction between two continuous covariates is simply their product.



# ------------------------------------------------------------------------------
# Finding Optimal model
# ------------------------------------------------------------------------------
step(M1)



# ----------
# Fit model M2
M2 <- lm(OrbitalV ~ LatAbs + CC + LatAbs : CC, data = HVS2)

print(summary(M2), digits = 3, signif.stars = FALSE)



# ----------
# Fit model M3
M3 <- lm(OrbitalV ~ LatAbs + CC , data = HVS2)

print(summary(M3), digits = 3, signif.stars = FALSE)



# ----------
# Degrees of freedom
X <- model.matrix(M2)

H <- X %*% solve(t(X) %*% X) %*% t(X)

sum(diag(H))


# Alternativly
sum(hatvalues(M2))



# ------------------------------------------------------------------------------
# Model validation
# ------------------------------------------------------------------------------

# residuals vs fitted values
E2 <- rstandard(M2)

F2 <- fitted(M2)

plot(x = F2, 
     y = E2, 
     xlab = "Fitted values",
     ylab = "Residuals")

abline(h = 0)



# ----------
# Cook distance plot
plot(cooks.distance(M2), 
     type = "h", 
     ylim = c(0, 1),
     ylab = "Cook distance values",
     xlab = "Observations",
     cex.lab = 1.5)

abline(h = 1, lwd = 2, lty = 2)



# ----------
# Normality
par(mfrow = c(1, 2), mar = c(5,5,2,2))
hist(E2, main = "", xlab = "Residuals", cex.lab = 1.5)
qqnorm(E2, main = "", cex.lab = 1.5)
qqline(E2)



# ----------
# Residuals vs each covariate in the model and not in the model
MyxyplotPolygon <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  Z <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(label = "Explanatory variables", cex = 1.5),
              ylab = "",
              strip = function(bg='white',cex.lab = 1.5,...)
                strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                t1 <- gam(y~s(x))
                MD1 <- data.frame(x=seq(from = min(x, na.rm = TRUE),
                                        to = max(x, na.rm = TRUE),
                                        length = 100))
                P1 <- predict(t1,   se.fit = TRUE)
                I1 <- order(x)
                xs <- sort(x)
                panel.lines(xs, P1$fit[I1], col = 1)
                panel.polygon(c(xs, rev(xs)),
                              c(P1$fit[I1]-2*P1$se.fit[I1],
                                rev(P1$fit[I1]+2*P1$se.fit[I1])),
                              col = gray(0.7),
                              density = 10 )
                panel.grid(h=-1, v= 2)
                panel.abline(0,0)
                panel.points(x, y, col = 1)
                
              })
  #Because the xyplot is inside a function you need to print 
  #construction below
  print(Z)
}

HVS2$E2 <- E2

MyVar   <- c("LatAbs", "CC", "FM",
             "Illuminance", "Temperature")

MyxyplotPolygon(HVS2, MyVar, "E2")



# -->
# The 95% point-wise confidence intervals for the smoothers contain 0 for all covariate values,
# indicating that there are no significant patterns in the residuals

