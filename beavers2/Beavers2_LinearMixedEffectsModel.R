# ------------------------------------------------------------------------------
# Data: Beavers
#
# Beavers have been reintroduced in Scotland. For such a program to succeed it is important to understand beaver foraging behavior.
# The white water lily is an important part of the beavers' diet.
# Approximately 20 healthy and ungrazed lily pads were removed from each Loch, and midlinedistance and petiole diameter for each specimen were determined.
# The objective:  develop a model describing midline diameter asn a function of petiole diameter
# 
# Clustered sample by Loch
# Reponse variable:  the midline diameter for the grazed lilies
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS", "VGAM", "car", "lme4")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/Beavers2.csv", header=T)
dim(data)
str(data)

data$MD <- data$Mid_line_Distance
data$PD <- data$Petiole_Diameter

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))

dim(data)
data2 <- na.exclude(data)
dim(data2)

# Note that we do not have data for 3 lochs for 2012
table(data$Loch, data$Year)


# ------------------------------------------------------------------------------
# Check linearlity by scatterplot
# ------------------------------------------------------------------------------
# not 100% sure whether this is a linear or non-linear pattern.
# If there is non-linearity, then this may be explained by other covariates (year, beaver presence, loch)
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = data2$PD, y = data2$MD, pch = 16, cex = 1,  ylab = "Midline Distance (mm)", xlab = "Petiole Diameter (mm)")
lines(lowess(x = data2$PD, y = data2$MD), col="red")



# ------------------------------------------------------------------------------
# Assess "pseudo-replication", independence assumption
# ------------------------------------------------------------------------------
# the MD values from some lochs and years are more similar than MD values from different lochs and years
data2$fYear <- factor(data2$Year)
p <- ggplot()
p <- p + xlab("Loch") + ylab("Midline Distance (mm)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_boxplot(data = data2, aes(x = Loch, y = MD))
p <- p + facet_grid( fYear ~ ., scales = "fixed")
p


# ------------------------------------------------------------------------------
# Mixed Effects Model:  imposing a dependency structure between all observations from the same cluster
#
# NOTE: WE NEED AT LEAST 5 CLUSTERS !!
# ------------------------------------------------------------------------------
# random intercept by Loch
M1 <- lmer(MD ~ PD + (1 | Loch), data = data2, REML= TRUE)
summary(M1)

# Fixed part coefficients
# beta in "E(MD) = mu = beta * PD + a"
summary(M1)$coef[,"Estimate"]
fixef(M1)
( beta <- fixef(M1) )

# Random part additional intercept by Loch
# "a" in E(MD) = mu = beta * PD + a"
ranef(M1)
( a <- ranef(M1)$Loch$'(Intercept)' )

# sigma in "MD ~ N(mu, sigma^2)"
( sigma_md <- summary(M1)$sigma )

# sigma in "a ~ N(0, sigma^2)
( sigma_a <- tidy(M1)[tidy(M1)$group == "Loch","estimate"] )

# Correlation between the intercept and the slope

# intra-class (Loch) correlation --> 0.33
# the correlation between any two observations from the same Loch is 0.33
# Difficult to state what constitutes a small enough value to omit the random effects all together.
# The current consensus is always to include the random intercepts base on the design of the experiment, even if it turns out that they are not important.
sigma_a^2 / ( sigma_a^2 + sigma_md^2 )


# Calcuate fitted value
X    <- model.matrix(~ PD, data = data2)
REff <- as.numeric(as.factor(data2$Loch))
Fit  <- X %*% beta + a[REff]
# same as fitted(M1)
fitted(M1)


# ------------------------------------------------------------------------------
# Assess heterogeneity and correlation
#
# We do not have the spatial coordinates of the sample, hence we cannot check the residuals for spacital correlation
# ------------------------------------------------------------------------------
E1 <- resid(M1)
F1 <- fitted(M1)

# No heterogeneity
par(mfrow = c(1, 1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Residuals", xlim = c(0, 220))
abline(h = 0, lty = 2, col = 1)


# ------------------------------------------------------------------------------
# Sketching the fitted value
# ------------------------------------------------------------------------------
MyData <- expand.grid(PD = seq(min(data2$PD), max(data2$PD), length = 25))
X      <- model.matrix(~ PD, data = MyData)

MyData$Pred <- X %*% fixef(M1)
MyData$SE   <- sqrt(  diag(X %*% vcov(M1) %*% t(X))  )
MyData$SeUp <- MyData$Pred + 1.96 * MyData$SE
MyData$SeLo <- MyData$Pred - 1.96 * MyData$SE
MyData

# Response value versus Covariate, fitted value and 95% confidence interval for the mean
p <- ggplot()
p <- p + geom_point(data = data2, aes(y = MD, x = PD), shape = 1, size = 1)
p <- p + xlab("Petiole diameter (mm)") + ylab("Midline diameter (mm)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_line(data = MyData, aes(x = PD, y = Pred), colour = "black")
p <- p + geom_ribbon(data = MyData, aes(x = PD, ymax = SeUp, ymin = SeLo ), alpha = 0.6)
p

# Random intercepts
data2$Fit <- fitted(M1)
p <- p + geom_line(data = data2, aes(x = PD, y = Fit, group = Loch), colour = "black")
p

