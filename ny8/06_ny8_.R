setwd("//media//kswada//MyFiles//R//ny8")

packages <- c("dplyr", "maptools", "spdep")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ny8
# ------------------------------------------------------------------------------
library(rgdal)
NY8 <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8_utm18.shp", "NY8_utm18")
TCE <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//TCE.shp", "TCE")

library(spdep)
NY_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY_nb.gal", region.id = row.names(NY8))
cities <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8cities.shp", "NY8cities")



# ----------
# subsetting only Syracuse city
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")

summary(Sy0_nb)



# ------------------------------------------------------------------------------
# Linear model for transformed incidence proportions 
# ------------------------------------------------------------------------------
# PEXPOSURE:  the inverse distance to the closest TCE
# PCTAGE65P:  the proportion of people aged 65 or higher
# PCTOWNHOME:  the proportion of people who own ther own home
# Z = log{ 1000 * ( Y(i) + 1 ) / n(i) }:  transformed incidence proportions

# most model fitting functions accept Spatial*DataFrame objects as their data argument values, and simply treat them as regular data.frame objects.
nylm <- lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8)

summary(nylm)



# ------------------------------------------------------------------------------
# Lagrange multiplier diagnostics for spatial dependence:
#
#   - The data generating process is a spatial error SAR or a spatial lag SAR ?
#   - spaial lag model includes only the endongenous spatially lagged dependent variable in the model
# ------------------------------------------------------------------------------
# lm.LMtests function here returns a list of five LM tests.

NYlistwW <- nb2listw(NY_nb, style = "W")

( res <- lm.LMtests(nylm, listw=NYlistwW, test="all") )

tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))

colnames(tres) <- c("Statistic", "df", "p-value")



# ----------
tres
printCoefmat(tres)


# -->
# LMerr test:  respond to both an amitted spatially lagged dependent variable and spatially autocorrelated residuals
# RLMerr (robust):  designed to test for spatially autocorrelated residuals in the possible presence of a ommitted spatially lagged dependent variable.

# These tests seem to point to a spatial lag specification.
# Further variants have been developed to take into account both spatial autocorrelation and heteroskedasticity.



# ------------------------------------------------------------------------------
# Fit a spatial lag model:  y = rho * W * y + X * beta + error  (where y is the endogenous variable, X is a matrix of exogeneous variables)
#   - by maximum likelihood, once again finding the spatial lag coeffs by line search,
#     then the remaining coeffs by generalised least squares.
#   - in this case, the implication of the spatial lag model is that the incidence rates by census tract
#     dependent on one-another directly, not a realistic hypothesis for this data set.
# ------------------------------------------------------------------------------

nylag <- lagsarlm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, listw=NYlistwW)

summary(nylag)

bptest.sarlm(nylag)




###################################################
### code chunk number 86: lat.Rnw:2942-2943
###################################################
library(McSpatial)


###################################################
### code chunk number 87: lat.Rnw:2945-2947
###################################################
McRes <- sarml(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, wmat=listw2mat(NYlistwW), eigvar=eigenw(NYlistwW), print=FALSE, data=NY8)
c(McRes$beta, rho=McRes$rho, sig2=McRes$sig2)


###################################################
### code chunk number 88: lat.Rnw:2963-2966
###################################################
nymix <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, type="mixed")
nymix
#anova(nymix, nylag)
LR.sarlm(nymix, nylag)


###################################################
### code chunk number 89: lat.Rnw:3048-3051
###################################################
W <- as(as_dgRMatrix_listw(NYlistwW), "CsparseMatrix")
trMat <- trW(W, type="mult")
head(trMat)


###################################################
### code chunk number 90: lat.Rnw:3088-3094
###################################################
set.seed(987654)
imps <- impacts(nymix, tr=trMat, R=1999)
imps
library(coda)
HPDinterval(imps, choice="direct")
HPDinterval(imps, choice="indirect")
HPDinterval(imps, choice="total")


###################################################
### code chunk number 91: lat.Rnw:3130-3133
###################################################
nyerr <- errorsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nyerr)
LR.sarlm(nyerr, nymix)


###################################################
### code chunk number 92: lat.Rnw:3153-3155
###################################################
nyerr1 <- errorsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, etype="emixed")
coef(nyerr1)


###################################################
### code chunk number 93: lat.Rnw:3186-3188
###################################################
set.seed(987654)
resMCMC <- MCMCsamp(nyerr1, mcmc=5000, burnin=500, listw=NYlistwW)


###################################################
### code chunk number 94: lat.Rnw:3197-3213
###################################################
oopar <- par(mfrow=c(1, 2), mar=c(3, 3, 3, 1))
plot(density(resMCMC[,3]), ylab="", main="Direct", xlab="", lwd=2)
lines(density(imps$sres$direct[,1]), lty=2, lwd=2)
abline(v=0)
plot(density(resMCMC[,6]), ylab="", main="Indirect", xlab="", lwd=2)
lines(density(imps$sres$indirect[,1]), lty=2, lwd=2)
abline(v=0)
legend("topright", legend=c("Error Durbin", "Durbin"), lty=1:2, bty="n", lwd=c(2, 2), cex=0.7)
par(oopar)


###################################################
### code chunk number 95: lat.Rnw:3243-3244
###################################################
library(sphet)


###################################################
### code chunk number 96: lat.Rnw:3246-3248
###################################################
nyGMlag <- spreg(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, listw=NYlistwW, model="lag", het=FALSE)
summary(nyGMlag)


###################################################
### code chunk number 97: lat.Rnw:3264-3266
###################################################
nyGMerr <- spreg(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, model="error", het=FALSE)
summary(nyGMerr)


###################################################
### code chunk number 98: lat.Rnw:3287-3288
###################################################
fit <- qregspiv(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, wmat=listw2mat(NYlistwW), data=NY8, tau=.5, nboot=200)


###################################################
### code chunk number 99: lat.Rnw:3347-3352
###################################################
library(mgcv)
NY8$x<-coordinates(NY8)[,1]/1000
NY8$y<-coordinates(NY8)[,2]/1000
nyGAM1 <- gam(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME+s(x,y), weights=POP8, data=NY8)
anova(nylmw, nyGAM1, test="Chisq")


###################################################
### code chunk number 100: lat.Rnw:3355-3356
###################################################
nylam1 <- c(summary(nyGAM1)$edf)


###################################################
### code chunk number 102: lat.Rnw:3401-3402
###################################################
nyGLMp <- glm(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8)), data=NY8, family="poisson")


###################################################
### code chunk number 103: lat.Rnw:3404-3405 
###################################################
summary(nyGLMp)


###################################################
### code chunk number 106: lat.Rnw:3421-3436
###################################################
library(RColorBrewer)
NY8$lmpresid <- residuals(nyGLMp, type="deviance")
gry <- c(rev(brewer.pal(6, "Reds")), brewer.pal(7, "Blues"))
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(NY8, "lmpresid", sp.layout=list(TCEpts), col.regions=gry, col="transparent", lwd=0.5, at=seq(-3,3.5,0.5))


###################################################
### code chunk number 108: lat.Rnw:3464-3466
###################################################
nyGAMp <- gam(Cases~PEXPOSURE+PCTAGE65P+PCTOWNHOME+offset(log(POP8))+s(x,y), data=NY8, family="poisson")
summary(nyGAMp)


###################################################
### code chunk number 109: lat.Rnw:3468-3469 
###################################################
anova(nyGLMp, nyGAMp, test="Chisq")


###################################################
### code chunk number 111: lat.Rnw:3479-3481
###################################################
nylam1 <- c(summary(nyGAMp)$edf)


