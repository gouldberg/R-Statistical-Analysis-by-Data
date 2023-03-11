# ------------------------------------------------------------------------------
# Data: Chinese Tallow Trees
# 
# full factorial design data
#  - 5 window durations, 2 competition levels, 2 fetilisation levels and 2 stress levels * 10 replicates = 5 * 2 * 2 * 2 * 10 = 400 pot
# Response variable:  the total stem and leaf biomass per pot  (!!! NOT COUNT DATA but CONTINUOUS DATA)
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom", "psych", "pscl")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/CTT_V2.csv", header=T)
dim(data)
str(data)

data <- data.frame(
  Window = data$window.duration,
  Stress = data$stress.type,
  Fertilization = data$fertilization,
  Competition = data$competition,
  Biomass = data$Triadica.aboveground.biomass)

Hmisc::describe(data)

# well-designed experiment
apply(data, MARGIN=2, table)


is.na(data)
colSums(is.na(data))


# ------------------------------------------------------------------------------
# Multi-panel Cleveland dotplots for all relevant variables
# ------------------------------------------------------------------------------
var <- c("Window", "Stress", "Fertilization", "Competition", "Biomass")

# Note that Draconarius has large number of zeros
dotplot(as.matrix(as.matrix(data[,var])),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))


# ------------------------------------------------------------------------------
# Check zero counts --> not many data excluding value 0  -->  multiple regression with 3-way interactions will consume df much -->  ZAG model
# ------------------------------------------------------------------------------
# zero counts --> only 95 observations are larger than 0
sum(data$Biomass == 0) / nrow(data)
sum(data$Biomass > 0) 

# rule of thumb for number of parameters for -->  6 to 9
sum(data$Biomass > 0) / c(10,15)

# ------------------------------------------------------------------------------
# Relationship of covariates to response variables
# ------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(Biomass ~ Window, data = data)
boxplot(Biomass ~ Stress, data = data)
boxplot(Biomass ~ Fertilization, data = data)
boxplot(Biomass ~ Competition, data = data)
plot(x = data$Window, y = data$Biomass)


# ------------------------------------------------------------------------------
# Multiple linear regression with all interactions
# ------------------------------------------------------------------------------
M1 <- lm(Biomass ~ (Window + Stress + Fertilization + Competition)^3, data = data)
summary(M1)

drop1(M1, test = "F")

# Model validation
E1 <- resid(M1)
F1 <- fitted(M1)

par(mfrow=c(1,1))
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)     

# Negative fitted values!
fitted(M1)

plot(x = fitted(M1), y = data$Biomass)
lines(lowess(x = fitted(M1), y = data$Biomass))


# Window versus Pearson Residuals
Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  library(mgcv)
  library(lattice)
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}

# looks OK
data$E1 <- resid(M1)
Myxyplot(data, "Window", "E1")

# Should we have used Window as a factor? --> No
boxplot(E1 ~ Window, data = data)
drop1(lm(E1 ~ factor(Window), data = data), test = "F")


# ------------------------------------------------------------------------------
# ZAG model:
# Applying the gamma GLM and Bernoulli GLM and Gluing together the two components
# ------------------------------------------------------------------------------
data.pos <- subset(data, Biomass > 0)
Zag1 <- glm(Biomass ~ Window + Fertilization + Stress + Competition, data = data.pos, family = Gamma(link = "log"))          
print(summary(Zag1), digits = 4)

# Bernoulli GLM
data$Biomass.01 <- as.numeric(data$Biomass > 0)
Zag2 <- glm(Biomass.01 ~ Window + Fertilization + Stress + Competition, data = data, family = binomial)          
summary(Zag2)
drop1(Zag2, test = "Chi")

# Calculate the ZAG expected values
# Bernoulli components
gamma <- coef(Zag2)
Xb <- model.matrix(~ Window + Fertilization +  Stress + Competition, data = data)
eta.binary <- Xb %*% gamma 
Pi <- exp(eta.binary) / (1 + exp(eta.binary))

# The gamma GLM component
beta  <- coef(Zag1)
Xc <- model.matrix(~ Window + Fertilization + Stress + Competition, data = data.pos)
eta.gamma  <- Xc %*% beta 
mu.gamma   <- exp(eta.gamma)


W  <- data$Biomass > 0
mu <- rep(1, nrow(data))
mu[W] <- mu.gamma
mu


# And calculate the ZAG (!!!!) expected values and variance:
r <- 1 / summary(Zag1)$dispersion
ExpY <- Pi * mu 
VarY <- (Pi  * r + Pi - Pi^2 * r) * mu^2 * (1 / r)

# And calculate the Pearson residuals:
E.zag <- (data$Biomass - ExpY) / sqrt(VarY)


# AIC of the ZAP GLMM:
AIC(Zag1) + AIC(Zag2)
# AIC(M1)


par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)

# Plot Pearson residuals versus Fitted Values
plot(x = ExpY, y = E.zag, xlab = "Fitted values", ylab = "Pearson residuals ZAG")
abline(h = 0, lty =2)

# Plot Pearson residuals versus Window Covariate
plot(x = jitter(data$Window), y = E.zag, xlab = "Window", ylab = "Pearson residuals")
abline(h = 0, lty =2)

# Plot Observed Response versus Window Covariate
plot(x = ExpY, y = data$Biomass, xlab = "Fitted values", ylab = "Observed biomass", xlim = c(0, 2.5), ylim = c(0, 2.5))


# ------------------------------------------------------------------------------
# ZAG model visulizing model fit (by Fertilization * Stress * Competition)
# ------------------------------------------------------------------------------
MyData <- expand.grid(Window = seq(4, 12, length = 100),
                      Fertilization = levels(data$Fertilization),
                      Competition = levels(data$Competition),
                      Stress = levels(data$Stress))


X <- model.matrix(~ Window + Fertilization + Stress + Competition, data = MyData)

mu   <- exp(X %*% beta)
Pi   <- exp(X %*% gamma) / (1 + exp(X %*% gamma))
ExpY <- Pi * mu


# Combine these three components with the MyData object
MyData2 <- cbind(MyData, mu, Pi, ExpY) 


head(MyData2)
library(ggplot2)

p <- ggplot()
p <- p + geom_jitter(data = data, aes(y = Biomass, x = Window), position = position_jitter(height = .01, width = 0.5))
p <- p + xlab("Window") + ylab("Biomass")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_line(data = MyData2, aes(x = Window, y = ExpY, group = Competition, colour = Competition, fill = Competition), lwd = 3)
p <- p + facet_grid(Fertilization ~ Stress, scales = "free_y")
p


# ------------------------------------------------------------------------------
# ZAG model visulizing model:  by three components
# ------------------------------------------------------------------------------
MyData3  <- MyData2[MyData2$Competition == "0",]

p1 <- ggplot()
p1 <- p1 + geom_jitter(data = data, aes(y = Biomass, x = Window), position = position_jitter(height = .01, width = 0.5))
p1 <- p1 + xlab("Window") + ylab("Expected biomass values")
p1 <- p1 + theme(text = element_text(size=15)) 

# Expected ZAG values
p1 <- p1 + geom_line(data = MyData3, aes(x = Window, y = ExpY), size = 2, col = 1)
p1 <- p1 + facet_grid(Fertilization ~ Stress, scales = "free_y")
p1

# quartz()  #Mac command for a new window. Use win.graph() on a Windows computer


p2 <- ggplot()
p2 <- p2 + geom_jitter(data = data,  aes(y = Biomass, x = Window), position = position_jitter(height = .01, width = 0.5))
p2 <- p2 + xlab("Window") + ylab("Expected mean Gamma part")
p2 <- p2 + theme(text = element_text(size=15)) 

# mu component
p2 <- p2 + geom_line(data = MyData3,aes(x = Window, y = mu), col = 3)
p2 <- p2 + facet_grid(Fertilization ~ Stress, scales = "free_y")
p2


# quartz()  #Mac command for a new window. Use win.graph() on a Windows computer


# Probability of presence
p3 <- ggplot()
p3 <- p3 + geom_jitter(data = data, aes(y = Biomass.01, x = Window), position = position_jitter(height = .01, width = 0.5))
p3 <- p3 + xlab("Window") + ylab("Probability of presence")
p3 <- p3 + theme(text = element_text(size=15)) 


# Pi components
p3 <- p3 + geom_line(data = MyData3, aes(x = Window, y = Pi), linetype = 2, col = 2)
p3 <- p3 + facet_grid(Fertilization ~ Stress, scales = "free_y")
p3

