# ------------------------------------------------------------------------------
# Data:  Antarcticbirds
#
# the aim of this case study:
#  - estimate trends in the arrival and laying dates in the three bird species
#  - analyse the differences between arrival and laying dates
#  - determine the effects of possible explanatory variables (e.g. ice vocer and the Southern Oscillation Index)
#
# SOI:  the Southern Oscillation Index, which represents the El Nino Southern Oscillation conditions.
#  - Many studies have shown that the El Nino Sourthern Oscillation (SOI) impacts on demographic rates and food resources of many animals, including seabirds.
#  - In addition, contrary to the proxy of sea ice extent, SOI is a alarge scale climate index that may affect seabirds, both during the breeding and non-breeding season.
#
# MSA:  Methanesulphonic acid is a product of biological activity in surface ocean water whose production is heavily influenced by the presence of sea ice in the Southern Ocean.
#  - An ice core from East Antarctica has reported a significant correlation between MSA and satellite-derived sea ice extent.
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom", "psych")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurDataMixedModelling/Antarcticbirds.txt", header=T, dec=".")
dim(data)
str(data)

# NOTE that many missing values
Hmisc::describe(data)
head(data, 20)


# ------------------------------------------------------------------------------
# Check the year trend of arriving and laying dates by species
# ------------------------------------------------------------------------------
Birds < -c(data$ArrivalAP, data$LayingAP, data$ArrivalCP, data$LayingCP, data$ArrivalEP, data$LayingEP)
AllYears < -rep(data$Year, 6)
MyNames <- c("Arrival Adelie Penguin", "Laying Adelie penguin", "Arrival Cape Petrel", "Laying Cape Petrel","Arrival Emperor Penguin", "Laying Emperor Penguin")
ID1 <- factor(rep(MyNames, each=length(data$Year)), levels=c(MyNames[1], MyNames[3], MyNames[5], MyNames[2], MyNames[4], MyNames[6]))

# show the patterns over time in arriving and laying dats for the three bird species.
# the timing of arrival in a vertain year may depend on the timing in the previous year and the same holds for laying dates.
xyplot(Birds ~ AllYears | ID1, xlab="Years", ylab="Day",
       layout=c(3,2),
       # strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "free")),
       panel=function(x,y){
         panel.xyplot(x,y,col=1)
         panel.loess(x,y,col=1,span=0.5)
         panel.grid(h=-1,v=2)})


# Auto-correlation function  --> LayingAP seems to have 1 year lag correlation
par(mfrow=c(2,1))
acf(data$ArrivalAP, lag.max = 10, na.action = na.pass, main="ArrivalAP")
acf(data$LayingAP, lag.max = 10, na.action = na.pass, main="LayingAP")

acf(data$LayingCP, lag.max = 10, na.action = na.pass, main="ArrivalCP")
acf(data$LayingCP, lag.max = 10, na.action = na.pass, main="LayingCP")

acf(data$LayingEP, lag.max = 10, na.action = na.pass, main="ArrivalEP")
acf(data$LayingEP, lag.max = 10, na.action = na.pass, main="LayingEP")


# no strong correlatiopn between Arrival date and Laying date
psych::pairs.panels(data[,2:7])


# ------------------------------------------------------------------------------
# Check the year trend of explanatory variables (MSA and SOI) and the dif of arriving and laying dates by species
# ------------------------------------------------------------------------------
data$DifAP <- data$LayingAP - data$ArrivalAP
data$DifCP <- data$LayingCP - data$ArrivalCP
data$DifEP <- data$LayingEP - data$ArrivalEP
AllDif <- c(data$DifAP, data$DifCP, data$DifEP, data$MSA, data$SOI)
AllYear <- rep(data$Year,5)
IDDif <- rep(c("Difference AP", "Difference CP", "Difference EP", "MSA", "SOI"), each=55)


# MSA has a clear trend over time and has a relative large number of missing values towards the end of the 1990s.
# no clear trend over in the difference time series for the species
xyplot(AllDif ~ AllYear | IDDif, xlab = "Years", ylab = "Day",
       layout=c(3, 2),
       # strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "free")),
       panel=function(x,y){
         panel.xyplot(x,y,col=1)
         panel.loess(x,y,col=1,span=0.5)
         panel.grid(h=-1,v=2)})


# ------------------------------------------------------------------------------
# Select optimal residual error structure ARMA(p, q) by AIC
#
# mgcv::gamm function does cross-validation and/or adding a residual auto-correlation structure to a smoothing model
# ------------------------------------------------------------------------------
library(mgcv)
library(nlme)

# Additive model with an ARMA error structure
# try all combinations for p and q from 0 to 1
var_n <- c("ArrivalAP", "LayingAP", "ArrivalCP", "LayingCP", "ArrivalEP", "LayingEP")
output <- data.frame()
ctrl <- lmeControl(opt='optim', optimMethod = 'SANN')

for(i in 1:length(var_n)){
  for(p in 0:1){
    for(q in 0:1){
      cat(paste0("processin: ", var_n[i], ": ", p, " - ", q, "\n"))
      if(p == 0 & q == 0){ eval(parse(text = paste0("tmp <- mgcv::gamm(", var_n[i], " ~ s(Year), data = data, control = ctrl)"))) }
      else eval(parse(text = paste0("tmp <- mgcv::gamm(", var_n[i], " ~ s(Year), correlation = corARMA(form=~Year, p=p, q=q), data = data, control = ctrl)")))
      output0 <- data.frame(var_n = var_n[i], p = p, q = q, AIC = AIC(tmp$lme))
      output <- rbind(output, output0)
    }
  }
}


# All six time series gave results where the optimal residual error structure was a ARMA(0,0), meaning that no correlation structure was needed
output
output %>% group_by(var_n) %>% mutate(rank = min_rank(AIC)) %>% filter(rank == 1)
tapply(X = output$AIC, INDEX = output$var_n, FUN = min)


# ------------------------------------------------------------------------------
# Using Ice Extent (MSA) as an explanatory variable
# ordinaly additive modeling
# ------------------------------------------------------------------------------
M_APA <- lm(ArrivalAP ~ MSA, data = data)
M_APL <- lm(LayingAP ~ MSA, data = data)
M_CPA <- lm(ArrivalCP ~ MSA, data = data)
M_CPL <- lm(LayingCP ~ MSA, data = data)
M_EPA <- lm(ArrivalEP ~ MSA, data = data)
M_EPL <- lm(LayingEP ~ MSA, data = data)

# only significant
# summary(M_APA)
summary(M_APL)
summary(M_CPA)
summary(M_CPL)
# summary(M_EPA)
summary(M_EPL)

Birds <- c(data$ArrivalAP, data$LayingAP, data$ArrivalCP, data$LayingCP, data$ArrivalEP, data$LayingEP)
MSA6 <- rep(data$MSA, 6)
ID6 <- rep(c("Arrival Adelie Penguin", "Laying Adelie penguin", "Arrival Cape Petrel", "Laying Cape Petrel", "Arrival Emperor Penguin", "Laying Emperor Penguin"), each = 55)

# The linear regression model showed that MSA has a negative effect on laying dates of all three birds and also on the arrival date of Cape Petrel
xyplot(Birds ~ MSA6 | ID6, xlab="MSA", ylab="Day",
       layout=c(3,2),
       # strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "free")),
       panel=function(x,y,subscripts,...){
         panel.xyplot(x,y,col=1)
         panel.grid(h=-1,v=2)
         I <- !is.na(y) & !is.na(x)
         tmp <- lm(y[I]~x[I]);  x1 <- x[I];
         y1 <- fitted(tmp);  I2 <- order(x1)
         panel.lines(x1[I2], y1[I2], col=1, span=1) })


# ------------------------------------------------------------------------------
# SOI and differences between arrival and laying dates
# Adelie Penguin
# ------------------------------------------------------------------------------
# The variation in arrival dates is considerably larger than for laying dates for all three bird species
Birds <- c(data$ArrivalAP, data$LayingAP, data$ArrivalCP, data$LayingCP, data$ArrivalEP, data$LayingEP)
ID6 <- rep(c("Arrival Adelie Penguin", "Laying Adelie penguin", "Arrival Cape Petrel", "Laying Cape Petrel", "Arrival Emperor Penguin", "Laying Emperor Penguin"), each = 55)
tmp <- data.frame(date = Birds, id = ID6)
bwplot(date ~ id, data = tmp)


# We model the arrival and laying dates for a bird simultaneously
AP <- c(data$ArrivalAP, data$LayingAP)
SOI2 <- c(data$SOI, data$SOI)
Y2 <- c(data$Year, data$Year)
ID <- factor(rep(c("Arrival", "Laying"), each = 55))

library(nlme)
vf2 <- varIdent(form =~ 1 | ID)
M_AP <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit)
M_AP2 <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, correlation = corAR1(form =~ Y2 | ID))

# The results show that there is a weak residual auto-correlation structure
anova(M_AP, M_AP2)


# Compare two models with the same random structure, but with different fixed effect. (need not REML but ML)
# We do not need interaction terms
M_AP3 <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, method = "ML", correlation = corAR1(form=~Y2|ID))
M_AP4 <- gls(AP ~ SOI2 + ID, weights = vf2, na.action = na.omit, method="ML", correlation=corAR1(form=~Y2|ID))
anova(M_AP3, M_AP4)


# No SOI effect for Adelie Penguins
summary(M_AP4)

M_AP5 <- gls(AP ~ ID, weights = vf2, na.action = na.omit, method = "ML", correlation=corAR1(form=~Y2|ID))
summary(M_AP5)


# Output shows
# - Predicted arrival time for Adelie Penguin is day 211 (rounded) and the laying date is 211 + 34 = 245
# - There is no effect of SOI on eigher arrival or laying dates
# - The residual standard error for the arrival dates is 3.36, but for the laying dats it is 0.6 smaller
# - There is also a small cmount of auto-correlation as rho = 0.26, meaning that the residual auto-correlation between two sequential years is equal to 0.26
#    and for time points that are separated by 2 years, this correlation is 0.26^2 = 0.07


# ------------------------------------------------------------------------------
# SOI and differences between arrival and laying dates
# Cape Petrel
# ------------------------------------------------------------------------------
CP <- c(data$ArrivalCP, data$LayingCP)

vf2 <- varIdent(form =~ 1 | ID)
M_CP <- gls(CP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit)
M_CP2 <- gls(CP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, correlation = corAR1(form =~ Y2 | ID))


# The results does not show that there is residual auto-correlation structure
anova(M_CP, M_CP2)


# We do not need interaction terms --> but we decided to keep it
M_CP3 <- gls(CP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, method = "ML")
M_CP4 <- gls(CP ~ SOI2 + ID, weights = vf2, na.action = na.omit, method = "ML")
anova(M_CP3, M_CP4)


# SOI effect and the nominal variale ID are highly significant
summary(M_CP3)

M_CP5 <- gls(CP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, method = "ML")
summary(M_CP5)


# Visualize Arrival and Laying date versus SOI
# Due to the weak interaction, the lines are nearly parallel indicating that there are no strong differences between the SOI-date relationship for arrival and laying
par(mfrow=c(1,1))
plot(data$SOI, data$ArrivalCP, ylim=c(195, 270), type="n", ylab="Arrival & laying dates")
points(data$SOI, data$ArrivalCP, pch=1)
points(data$SOI, data$LayingCP, pch=2)
MyX = data.frame(SOI2=seq(from=min(data$SOI), to=max(data$SOI), length=20), ID="Arrival")
Pred1 <- predict(M_CP5, newdata=MyX)
lines(MyX$SOI2, Pred1)
MyX = data.frame(SOI2=seq(from=min(data$SOI),to=max(data$SOI), length=20), ID="Laying")
Pred2 <- predict(M_CP5, newdata=MyX)
lines(MyX$SOI2, Pred2)


# ------------------------------------------------------------------------------
# SOI and differences between arrival and laying dates
# Emperor Penguin
# ------------------------------------------------------------------------------
EP <- c(data$ArrivalEP, data$LayingEP)

vf2 <- varIdent(form =~ 1 | ID)
M_EP <- gls(EP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit)
M_EP2 <- gls(EP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, correlation = corAR1(form =~ Y2 | ID))

# The results show that there is a weak residual auto-correlation structure
anova(M_EP, M_EP2)


# Compare two models with the same random structure, but with different fixed effect. (need not REML but ML)
# We do not need interaction terms
M_EP3 <- gls(EP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, method = "ML", correlation = corAR1(form=~Y2|ID))
M_EP4 <- gls(EP ~ SOI2 + ID, weights = vf2, na.action = na.omit, method="ML", correlation=corAR1(form=~Y2|ID))
anova(M_EP3, M_EP4)


# No SOI effect
summary(M_EP4)

M_EP5 <- gls(EP ~ ID, weights = vf2, na.action = na.omit, method = "ML", correlation=corAR1(form=~Y2|ID))
summary(M_EP5)

# Output shows
# - Predicted difference between arrival date and laying date for Emperor Penguin is day 52 (rounded)
# - There is no effect of SOI on eigher arrival or laying dates
# - The residual standard error for the arrival dates is 5.07, but for the laying dats it is 0.54 smaller
# - There is also a small cmount of auto-correlation as rho = 0.20, meaning that the residual auto-correlation between two sequential years is equal to 0.20
#    and for time points that are separated by 2 years, this correlation is 0.20^2 = 0.04


