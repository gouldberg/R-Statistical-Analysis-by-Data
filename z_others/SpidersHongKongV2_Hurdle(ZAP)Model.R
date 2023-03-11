# ------------------------------------------------------------------------------
# Data:  Spiders Hong Kong V2
# 
# Count data on ground-dwelling riparian spiders along two Hong Kong forest streams
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom", "psych", "pscl")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/SpidersHongKongV2.csv", header=T)
dim(data)
str(data)

Hmisc::describe(data)


# ------------------------------------------------------------------------------
# Multi-panel Cleveland dotplots for all relevant variables
# ------------------------------------------------------------------------------
var <- c("Season", "Site", "Distance", "Draconarius", "Ti",           
           "Ti_dw", "SoilMoisture", 
           "Leaf")

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
# Try (BoxCox) Transformation
# ------------------------------------------------------------------------------
source("./utilities/BoxCoxTransInvestigate.R")

BoxCoxTrans_byvar(data, var)


# ------------------------------------------------------------------------------
# Scatterplots with Pearson correlation coef of all relevant variables
# Checking:  collinearilty
# ------------------------------------------------------------------------------
data$LogLeaf <- log(data$Leaf)
var <- c("Draconarius", "Distance", "Season", 
           "Ti", "Ti_dw", "SoilMoisture", "LogLeaf")

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

# Distance and LogLeaf biomass are wekaly collinear.
pairs(data[,var], labels = var,
      cex.labels =  2,
      lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
      panel.cor(x, y, digits, prefix, cex.cor)}, 
      upper.panel =  function(x, y) points(x, y, 
                                           pch = 16, cex = 0.8, 
                                           col = gray(0.1)))

# Checking VIF --> Not clear collinearity
source("./utilities/CorVif.R")
var <- c("Distance", "Season", 
           "Ti", "Ti_dw", "SoilMoisture", "LogLeaf")
corvif(data, var)


# Data transformation:  log (for large value) and sqrt (for small value)
data$LogTi   <- log(data$Ti + 1)
data$SQTidw  <- sqrt(data$Ti_dw)
data$fSite <- factor(data$Site, levels = c(1,2), labels = c("Site 1", "Site 2"))
data$fSeason <- factor(data$Season, levels = c(1,2), labels = c("Season 1", "Season 2"))
data$fDistance <- factor(data$Distance, levels = c(0, 2, 5, 10), labels = c("0 m", "2 m", "5 m", "10 m"))


# CHECK COLLINEARITY by MORE SEGMENTATION (per site and per distance)
p <- ggplot()
p <- p + xlab("Log transformed Ti") + ylab("Square root transformed Ti_dw")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_point(data = data, aes(x = LogTi, y = SQTidw))
p <- p + geom_smooth(data = data, aes(x = LogTi, y = SQTidw), method = lm)
p <- p + facet_grid(fSite ~ fSeason, scales = "fixed")
p


# ------------------------------------------------------------------------------
# Check distance variable
# ------------------------------------------------------------------------------
# Distance and counts
# Distance has only 4 values, and boxplots indicate that there is potentially a non-linear effect of distance, and we therefore opt for the categorical approach.
p <- ggplot()
p <- p + xlab("Distance") + ylab("Counts")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_boxplot(data = data, aes(x = fDistance, y = Draconarius))
p <- p + facet_grid(fSite ~ fSeason, scales = "fixed")
p


# ------------------------------------------------------------------------------
# Check zero counts
# ------------------------------------------------------------------------------
# zero counts
sum(data$Draconarius == 0) / nrow(data)
sum(data$Draconarius > 0) 


# ------------------------------------------------------------------------------
# Check potential dependency in the abundance data.
# Spatial position of the selected quadrat locations per day, site, season and distance  (how the data collected)
# ------------------------------------------------------------------------------
data$Date
data$Time <- as.Date(data$Date, "%d/%m/%y")

p <- ggplot()
p <- p + xlab("Time (week number)") + ylab("Sampled quadrats")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_point(data = data,  aes(x = Time, y = Quadrat))

library(scales) 
p <- p + scale_x_date(labels = date_format("%W"), breaks = date_breaks("months"))
p <- p + facet_grid(Distance ~ fSite * fSeason, scales = "free")
p


# Number of Draconarius spp. counts plotted versus time for each season
p <- ggplot()
p <- p + xlab("Time") + ylab("Counts")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_jitter(data = data, aes(x = Time, y = Draconarius), position = position_jitter(width = 1, height = 0))

library(scales) 
p <- p + scale_x_date(labels = date_format("%d/%m/%y"), breaks = date_breaks("months"))
p <- p + geom_smooth(data = data, aes(x = Time, y = Draconarius))
p <- p + facet_grid(fSeason ~ ., scales = "fixed")
p


# Number of Draconarius spp. counts plotted versus time for each season and site, and each distance
p <- ggplot()
p <- p + xlab("Time (week number)") + ylab("Counts")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_jitter(data = data, aes(x = Time, y = Draconarius), position = position_jitter(width = 1, height = 0))

library(scales) 
p <- p + scale_x_date(labels = date_format("%W"),  breaks = date_breaks("months"))
p <- p + geom_smooth(data = data, aes(x = Time, y = Draconarius), se = FALSE, col = 1, span = 0.8)
p <- p + facet_grid(fDistance ~ fSite * fSeason, scales = "free")
p


# ------------------------------------------------------------------------------
# Poisson GLM and check dispersion statistic
# ------------------------------------------------------------------------------
# Just Poisson Regression
M1 <- glm(Draconarius ~ fSite + fDistance + LogLeaf + SQTidw, family = poisson, data = data)
summary(M1)

# dispersion statistic
E1 <- resid(M1, type = "pearson")
N <- nrow(data);  P <- length(coef(M1))
sum(E1^2) / (N - P)


# ------------------------------------------------------------------------------
# Poisson GLM:  check residuals
# ------------------------------------------------------------------------------
# Pearson Residuals versus fitted values
F1 <- fitted(M1)
par(cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h = 0, lty = 2)     

# Pearson Residuals versus distance
boxplot(E1 ~ fDistance, data = data)

# Pearson Residuals versus distnace by season and site
p <- ggplot()
p <- p + xlab("Distance") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_boxplot(data = data, aes(x = fDistance, y = E1))
p <- p + facet_grid(fSite ~ fSeason, scales = "fixed")
p

# Pearson Residuals versus other covariates
data$E1 <- E1
var <- c("LogLeaf", "LogTi", "SQTidw")
Myxyplot(data, var, "E1", MyXlab = "Covariates", MyYlab = "Pearson residuals")

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


# ------------------------------------------------------------------------------
# Poisson GLM:  check spatial and temporal correlation by Variogram
#
#  --> no spatial and temporal correlation --> we suspect that zeros are causing the overdispersion !!!
# ------------------------------------------------------------------------------
library(gstat)
library(sp)

# Spatial correlation:  No correlation
# Transfer Longitude and Latitude to UTM coordinates
MyData <- data.frame(E1 = E1, Quadrat = data$Quadrat, MyOnes  = rep(1, nrow(data)))
coordinates(MyData) <- c("Quadrat", "MyOnes")
V1 <- variogram(E1 ~ 1, MyData)
plot(V1)

# Temporal correlation:  No correlation
Time <- as.Date(data$Time, "%y/%m/%d")
TimeNum <- as.numeric(Time) - min(as.numeric(Time)) + 1
#Transfer Longitude and Latitude to UTM coordinates
MyData <- data.frame(E1 = E1, TimeNum = TimeNum, MyOnes  = rep(1, nrow(data)))
coordinates(MyData) <- c("TimeNum", "MyOnes")
V1 <- variogram(E1 ~ 1, MyData, cutoff = 30)
plot(V1)


# Pearson Residuals versus time
boxplot(E1 ~ Time, data = data)
boxplot(E1 ~ TimeNum, data = data)
plot(x = TimeNum, y = E1)

p <- ggplot()
p <- p + xlab("Time (week number)") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_jitter(data = data, aes(x = Time, y = E1), position = position_jitter(width = 1, height = 0))
library(scales) 
p <- p + scale_x_date(labels = date_format("%W"), breaks = date_breaks("months"))
p <- p + geom_smooth(data = data, aes(x = Time, y = E1), se = FALSE, col = 1, span = 0.8)
p <- p + facet_grid(. ~  fSeason, scales = "free")
p


# Pearson Residuals versus Quadrat
p <- ggplot()
p <- p + xlab("Quadrat") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_jitter(data = data, aes(x = Quadrat, y = E1), position = position_jitter(width = 1, height = 0))
library(scales) 
p <- p + geom_smooth(data = data, aes(x = Quadrat, y = E1), se = TRUE, col = 1, span = 0.8)
p <- p + facet_grid(. ~  fDistance, scales = "free")
p


# ------------------------------------------------------------------------------
# Hurdle Model (Zero-Altered Poisson Model) 
# ------------------------------------------------------------------------------
library(pscl)
M2 <- hurdle(Draconarius ~ fSite + fDistance + LogLeaf + SQTidw | fSite + fDistance + LogLeaf + SQTidw, dist = "poisson", data = data)
summary(M2)


# dispersion statistic  --> small enough to ignore
E2 <- resid(M2, type = "pearson")
N <- nrow(data)
P <- length(coef(M2))
sum(E2^2) / ( N - P )


# LR test to assess the significance of distance (truncated poisson part) --> weakly significant
M2a <- hurdle(Draconarius ~ fSite + LogLeaf + SQTidw | fSite + fDistance + LogLeaf + SQTidw, dist = "poisson", data = data)
1 - pchisq(as.numeric(logLik(M2) - logLik(M2a)), 3)

# LR test to assess the significance of distance (binary part) --> weakly significant
M2b <- hurdle(Draconarius ~ fSite + fDistance + LogLeaf + SQTidw | fSite + LogLeaf + SQTidw, dist = "poisson", data = data)
1 - pchisq(as.numeric(logLik(M2) - logLik(M2b)), 3)


# ------------------------------------------------------------------------------
# Find optimal model by AIC backword selection (no step function)
# ------------------------------------------------------------------------------
f1 <- Draconarius ~ fSite + fDistance  + LogLeaf + SQTidw |
  fSite + fDistance  + LogLeaf + SQTidw

f1a <- Draconarius ~        fDistance  + LogLeaf + SQTidw |
  fSite + fDistance  + LogLeaf + SQTidw

f1b <- Draconarius ~ fSite             + LogLeaf + SQTidw |
  fSite + fDistance  + LogLeaf + SQTidw

f1c <- Draconarius ~ fSite + fDistance           + SQTidw |
  fSite + fDistance  + LogLeaf + SQTidw

f1d <- Draconarius ~ fSite + fDistance  + LogLeaf         |
  fSite + fDistance  + LogLeaf + SQTidw

f1e <- Draconarius ~ fSite + fDistance  + LogLeaf + SQTidw |
  fDistance  + LogLeaf + SQTidw

f1f <- Draconarius ~ fSite + fDistance  + LogLeaf + SQTidw |
  fSite               + LogLeaf + SQTidw

f1g <- Draconarius ~ fSite + fDistance  + LogLeaf + SQTidw |
  fSite + fDistance             + SQTidw

f1h <- Draconarius ~ fSite + fDistance  + LogLeaf + SQTidw |
  fSite + fDistance  + LogLeaf 


M2 <- hurdle(f1, dist = "poisson", data = data)
M2a <- hurdle(f1a, dist = "poisson", data = data)
M2b <- hurdle(f1b, dist = "poisson", data = data)
M2c <- hurdle(f1c, dist = "poisson", data = data)
M2d <- hurdle(f1d, dist = "poisson", data = data)
M2e <- hurdle(f1e, dist = "poisson", data = data)
M2f <- hurdle(f1f, dist = "poisson", data = data)
M2g <- hurdle(f1g, dist = "poisson", data = data)
M2h <- hurdle(f1h, dist = "poisson", data = data)

AIC(M2, M2a, M2b, M2c, M2d, M2e, M2f, M2g, M2h)


# optimal model
# 5m or 10m away from the stream means larger abundances, positive effect of SQT terrestrial arthropod dry weight
# site 2 has higher probability of presence, positive effect of SQT terrestrial arthropod dry weight on the probability of presence
M5 <- hurdle(Draconarius ~ fDistance + SQTidw | fSite + fDistance + SQTidw, dist = "poisson", data = data)
summary(M5)

# dispersion statistic  --> small enough to ignore
E5 <- resid(M5, type = "pearson")
N <- nrow(data)
P <- length(coef(M5))
sum(E5^2) / ( N - P )

# model comparison of ZIP and ZAP
AIC(M1, M5)



beta  <- coef(M5, model = "count")
gamma <- coef(M5, model = "zero")
X <- model.matrix(~ fDistance + SQTidw, data = data)
Z <- model.matrix(~ fSite + fDistance + SQTidw, data = data)
mu            <- exp(X %*% beta)
mu.ZTruncPois <-  mu / (1 - exp(-mu))
Pi.zero       <- exp(Z %*% gamma) / (1 + exp(Z %*% gamma))
MyFit <- Pi.zero * mu.ZTruncPois 
fitted(M5) - MyFit

data$Draconarius01 <- as.numeric(data$Draconarius > 0)
T01 <- glm(Draconarius01 ~  fSite + fDistance + SQTidw, family = binomial, data = data)


library(glmmADMB)        
dataPos <- subset(data, Draconarius > 0)   
TPos <- glmmadmb(Draconarius ~  fDistance + SQTidw, family = "truncpoiss", data = dataPos)
beta.self <- fixef(TPos)
gamma.self <- coef(T01)
mu            <- exp(X %*% beta.self)
mu.ZTruncPois <-  mu / (1 - exp(-mu))
Pi.zero       <- exp(Z %*% gamma.self) / (1 + exp(Z %*% gamma.self))
MyFitSelf     <- Pi.zero * mu.ZTruncPois 


cbind(fitted(M5), MyFit, MyFitSelf)


# ------------------------------------------------------------------------------
# Visualize fitted values / probabilities for each part model
# ------------------------------------------------------------------------------
beta  <- coef(M5, model = "count")
gamma <- coef(M5, model = "zero")


# Fitted values of the zero-truncated count part of the ZAP
library(plyr)
data2 <- subset(data, Draconarius > 0)
MyDataX <- ddply(data2, .(fDistance), summarize, SQTidw = seq(min(SQTidw), max(SQTidw), length = 25))
X <- model.matrix(~fDistance + SQTidw, data = MyDataX)
mu.pois    <- exp(X %*% beta)
MyDataX$mu <- mu.pois / (1 - exp(-mu.pois))

p <- ggplot()
p <- p + xlab("SQTidw") + ylab("Count part")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_point(data = data2, aes(x = SQTidw, y = Draconarius))
p <- p + geom_line(data = MyDataX, aes(x = SQTidw, y = mu), col = 1)
p <- p + facet_grid(. ~ fDistance, scales = "free")
p


# Fitted probabilities for the binary component of the ZAP
MyDataZ <- ddply(data, .(fSite, fDistance), summarize, SQTidw = seq(min(SQTidw), max(SQTidw), length = 25))
Z <- model.matrix(~fSite + fDistance + SQTidw, data = MyDataZ)
MyDataZ$Pi <- exp(Z %*% gamma) / (1 + exp(Z %*% gamma))
data$Draconarius01 <- as.numeric(data$Draconarius > 0)
p <- ggplot()
p <- p + xlab("SQTidw") + ylab("Probability of presence")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_point(data = data, aes(x = SQTidw, y = Draconarius01))
p <- p + geom_line(data = MyDataZ, aes(x = SQTidw, y = Pi), col = 1)
p <- p + facet_grid(fSite ~ fDistance, scales = "free")
p


# ------------------------------------------------------------------------------
# Visualize expected values for the ZAP model (and poisson GLM to be compared)
# ------------------------------------------------------------------------------
ExpY1        <- MyDataX$mu * MyDataZ$Pi[MyDataZ$fSite == "Site 1"] 
ExpY2        <- MyDataX$mu * MyDataZ$Pi[MyDataZ$fSite == "Site 2"] 
MyDataZ$ExpY <- c(ExpY1, ExpY2)
p <- ggplot()
p <- p + xlab("SQTidw") + ylab("Expected values of the ZAP model")
p <- p + theme(text = element_text(size = 15), legend.position = "none")
p <- p + geom_point(data = data2, aes(x = SQTidw, y = Draconarius))
p <- p + geom_line(data = MyDataZ,aes(x = SQTidw, y = MyDataZ$ExpY), col = 1, lwd = 2)
p <- p + facet_grid(fSite ~ fDistance, scales = "free")
p
# And add the Poisson GLM fitted values
M6 <- glm(Draconarius ~ fSite + fDistance + SQTidw, family = poisson, data = data)
MyDataPois <- ddply(data, .(fSite , fDistance), summarize, SQTidw = seq(min(SQTidw),max(SQTidw), length = 25))
XPois <- model.matrix(~fSite + fDistance + SQTidw, data = MyDataPois)
MyDataPois$mu <- exp(XPois %*% coef(M6))
p <- p + geom_line(data = MyDataZ, aes(x = SQTidw, y = MyDataPois$mu), col = 2, lwd = 1)
p


# compare poisson GLM and ZAP model
plot(resid(M6), resid(M5))
plot(fitted(M6), fitted(M5))


# ------------------------------------------------------------------------------
# Visualize density curves and expected values for the ZAP model (and poisson GLM to be compared) in 3D
# ------------------------------------------------------------------------------
# Density curves for the Poisson GLM showing the probababilities of other Draconarius spp. values
XPois <- model.matrix(~fSite + fDistance + SQTidw, data = MyDataPois)
MyDataPois$mu <- exp(XPois %*% coef(M6))
xyplot(mu ~ SQTidw |  fSite * fDistance,data = MyDataPois,type = "l")
library(scatterplot3d) 
MyData11 <- subset(MyDataPois, subset = (fSite == "Site 1" & fDistance == "10 m"))
dim(MyData11)
#Three variable for 3-d scatterplot
x <- MyData11$SQTidw
y <- MyData11$mu
z <- 0 * x
rr<- scatterplot3d(x, y, z, 
                   highlight.3d = FALSE, 
                   col.axis = "black",
                   col.grid = "black", 
                   pch = 20,
                   zlim = c(0, 1),
                   ylim = c(0, 15),
                   type="l",
                   lwd = 3,
                   grid = FALSE,
                   box = TRUE,
                   cex.lab = 1.5,
                   xlab = "SQTidw",
                   ylab = "Possible values",
                   zlab = "Probability")
#Pick 5 values along the x axis to plot the density curves.
MyDatai <- data.frame(fSite = factor("Site 1", levels = levels(data$fSite)),
                      fDistance = factor("10 m", levels = levels(data$fDistance)),
                      SQTidw = seq(from = quantile(data$SQTidw, 0.15), to =   0.9 * max(data$SQTidw), length = 4))
I <- data$fSite == "Site 1" & data$fDistance == "10 m"
data.11 <- data[I,]
for (i in 1:4){
  Xi <- model.matrix(~fSite + fDistance + SQTidw, data = MyDatai[i,])
  mu.i = exp(Xi %*% coef(M6))
  yseq = round(seq(0, 15, by = 1))
  zi = dpois(yseq, lambda = mu.i)
  rb = cbind(Xi[1, "SQTidw"], yseq, zi)
  rr$points3d(rb, col = 1, type = "h", pch = ".", lwd = 2)
  #Dotted line 
  rb = cbind(Xi[1, "SQTidw"], yseq, 0)
  rr$points3d(rb, col = 1, type = "l", pch = ".", lty = 2)
  rdat <- cbind(data.11$SQTidw, data.11$Draconarius, rep(0,nrow(data.11)))
  rr$points3d(rdat, col = 1, type = "p", pch = 16, cex = 1)
}



# Density curves for the ZAP model showing the probababilities of other Draconarius spp. values
MyData <- ddply(data, .(fSite , fDistance), summarize, SQTidw = seq(min(SQTidw),  max(SQTidw), length = 25))
X <- model.matrix(~fDistance + SQTidw, data = MyData)
mu.pois    <- exp(X %*% beta)
mu.ZeroTruncPois <- mu.pois / (1 - exp(-mu.pois))
Z  <- model.matrix(~fSite + fDistance + SQTidw, data = MyData)
Pi <- exp(Z %*% gamma) / (1 + exp(Z %*% gamma))
MyData$ExpY  <- Pi * mu.ZeroTruncPois  
#Focus on one of the four scenarios
MyData11 <- subset(MyData, subset = (fSite == "Site 1" & fDistance == "10 m"))
dim(MyData11)
#Three variable for 3-d scatterplot
x <- MyData11$SQTidw
y <- MyData11$ExpY
z <- 0 * x
rr<- scatterplot3d(x, y, z, 
                   highlight.3d = FALSE, 
                   col.axis = "black",
                   col.grid = "black", 
                   pch = 20,
                   zlim = c(0, 1),
                   ylim = c(0, 15),
                   type="l",
                   lwd = 3,
                   grid = FALSE,
                   box = TRUE,
                   cex.lab = 1.5,
                   xlab = "SQTidw",
                   ylab = "Possible values",
                   zlab = "Probability")

#Pick values along the x axis to plot the density curves.
MyDatai <- data.frame(fSite = factor("Site 1", levels = levels(data$fSite)),
                      fDistance = factor("10 m", levels = levels(data$fDistance)),
                      SQTidw = seq(from = quantile(data$SQTidw, 0.15), to =   0.9 * max(data$SQTidw), length = 4))
I <- data$fSite == "Site 1" & data$fDistance == "10 m"
data.11 <- data[I,]
for (i in 1:4){
  Xi <- model.matrix(~fDistance + SQTidw, data = MyDatai[i,])
  mu.ZeroTruncPois <- exp(Xi %*% beta) / (1 - exp(-exp(Xi %*% beta)))
  Zi    <- model.matrix(~fSite + fDistance + SQTidw, data = MyDatai[i,])
  Pi    <- exp(Zi %*% gamma) / (1 + exp(Zi %*% gamma))
  mu.i  <- Pi * mu.ZeroTruncPois  
  yseq <- round(seq(0, 15, by = 1))
  zi   <- dzapois(yseq, lambda = mu.ZeroTruncPois, pobs0 = 1 - Pi)
  rb = cbind(Xi[1, "SQTidw"], yseq, zi)
  rr$points3d(rb, col = 1, type = "h", pch = ".", lwd = 2)
  #Dotted line 
  rb = cbind(Xi[1, "SQTidw"], yseq, 0)
  rr$points3d(rb, col = 1, type = "l", pch = ".", lty = 2)
  rdat <- cbind(data.11$SQTidw, data.11$Draconarius, rep(0,nrow(data.11)))
  rr$points3d(rdat, col = 1, type = "p", pch = 16, cex = 1)
}


# ------------------------------------------------------------------------------
# Simulating from the model:  assess model validity by percentage of zeros from model vs. observed data
# the Frequency of the percentage of zeros in 1000 simulated datasets from models
# BIG RED DOT is the number of zeros for the observed data.
# ------------------------------------------------------------------------------
par(mar = c(5,5,2,2), cex.lab = 1.5, mfrow = c(1,2))

N     <- nrow(data)
muP   <- fitted(M6)
YPois <- matrix(nrow = N, ncol = 1000)
zeros <- vector(length = 1000)
for(i in 1:1000){
  YPois[,i] <- rpois(N, lambda = muP)
  zeros[i] <- sum(YPois[,i] == 0) / N
}

plot(table(zeros), 
     #xlim = c(0, 200 / N),
     axes = FALSE,
     xlab = "Percentage of zeros",
     ylab = "Frequency",
     xlim = c(0.4, 0.8),
     ylim = c(0, 70))
axis(2)
axis(1, at = c(0.4, 0.5, 0.6, 0.7, 0.8), labels = c("40%", "50%", "60%", "70%", "80%"))     
points(x = sum(data$Draconarius == 0) / N, y = 0, pch = 16, cex = 5, col = 2)
text(0.42, 67, "A", cex = 1.5)


#B. And the same for the ZAP:
Xzap <- model.matrix(~ fDistance + SQTidw, data = data)
Zzap <- model.matrix(~ fSite + fDistance + SQTidw, data = data)
muzap <- exp(Xzap %*% beta)
Pizap <- exp(Zzap %*% gamma) / (1 + exp(Zzap %*% gamma))
N <- nrow(data)
YZap <- matrix(nrow = N, ncol = 1000)
YPoi <- matrix(nrow = N, ncol = 1000)
zeros <- vector(length = 1000)
for(i in 1:1000){
  YZap[,i] <- rzapois(N, lambda = muzap, pobs0 = 1 - Pizap)
  zeros[i] <- sum(YZap[,i] == 0) / N
}
plot(table(zeros), 
     xlim = c(0.4, 0.8),
     axes = FALSE,
     xlab = "Percentage of zeros",
     ylab = "Frequency",
     ylim = c(0, 70))
axis(2)
axis(1, at = c(0.4, 0.5, 0.6, 0.7, 0.8), labels = c("40%", "50%", "60%", "70%", "80%"))     
points(x = sum(data$Draconarius == 0) / N, y = 0, pch = 16, cex = 5, col = 2)
text(0.42, 67, "B", cex = 1.5)


# ------------------------------------------------------------------------------
# Simulating from the model:  assess model validity by average counts from model vs. observed data
# Thin line:  average counts from simulated data    Thick line:  Observed data
# Poisson GLM does not generate enough zeros and too many ones.
# ------------------------------------------------------------------------------
par(mar = c(5,5,2,2), cex.lab = 1.5, mfrow = c(1,2))

Z <- matrix(nrow = max(YZap)+1, ncol = 1000)
for (i in 1:1000){
  zi <- table(YPois[,i])
  I <- as.numeric(names(zi)) + 1
  Z[I,i] <- zi
}
Z[is.na(Z)] <- 0
Z[,1:5]
rowSums(Z) / 1000


Xi <- 0: max(YZap)
AverageTable <- rowSums(Z) / 1000
plot(x = Xi, 
     y = AverageTable,
     type = "h",
     ylim = c(0, 200),
     lwd = 2,
     xlab = "Draconarius values",
     ylab = "Frequencies")
text(0, 195, "A", cex = 1.5)



Zs <- table(data$Draconarius)
nx <- length(Zs)
NamesZ <- as.numeric(names(Zs))
for (i in 1:nx){
  segments(x0 = NamesZ[i] + 0.3,
           x1 = NamesZ[i] + 0.3,
           y0 = 0,
           y1 = Zs[i],
           lwd = 7)
}

Z <- matrix(nrow = max(YZap)+1, ncol = 1000)
for (i in 1:1000){
  zi <- table(YZap[,i])
  I <- as.numeric(names(zi)) + 1
  Z[I,i] <- zi
}
Z[is.na(Z)] <- 0
Z[,1:5]
rowSums(Z) / 1000

Xi <- 0: max(YZap)
AverageTable <- rowSums(Z) / 1000
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Xi, 
     y = AverageTable,
     type = "h",
     ylim = c(0, 200),
     lwd = 2,
     xlab = "Draconarius values",
     ylab = "Frequencies")
text(0, 195, "B", cex = 1.5)

Zs <- table(data$Draconarius)
nx <- length(Zs)
NamesZ <- as.numeric(names(Zs))
for (i in 1:nx){
  segments(x0 = NamesZ[i] + 0.3,
           x1 = NamesZ[i] + 0.3,
           y0 = 0,
           y1 = Zs[i],
           lwd = 7)
}





