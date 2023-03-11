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
# CAR (Conditional Autoregressive Model)
#   - The CAR specification relies on the conditional distribution of the spatial error terms.
#     Instead of the whole e(-i) vector, only the neighbours of area i, defined in a chosen way, are used.
#   - However, specifying the conditional distributions of the error terms does not imply that the joint distribution exists.
#     To have a proper distribution some constraints must be set on the parameters of the model.
# ------------------------------------------------------------------------------
nycar <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME , data=NY8, family="CAR", listw=NYlistw)

summary(nycar)


# -->
# The estimated coeffs of the covariates in the model are very similar to those obtained with the SAR models.
# Nevertheless, the p-values of two covariates, the distance to the nearest TCE and the percentage of people owning a home,
# are slighly above the 0.05 threshold.

# The likelihood ratio test indicates that there is significant spatial autocorrelation and the estimated value of lambda is 0.0841.



# ------------------------------------------------------------------------------
# CAR (Conditional Autoregressive Model) taking account into population size as weights
# ------------------------------------------------------------------------------
nylam1 <- c(nycar$lambda)


nycarw <- spatialreg::spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, family="CAR", listw=NYlistw, weights=POP8)
summary(nycarw)


###################################################
### code chunk number 77: lat.Rnw:2624-2626
###################################################
nysarwM<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
                              listw=NYlistw, weights=POP8, method="Matrix")


###################################################
### code chunk number 78: lat.Rnw:2628-2629
###################################################
summary(nysarwM)


###################################################
### code chunk number 79: lat.Rnw:2659-2664
###################################################
1/range(eigenw(NYlistw))
nysar_ll<-spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
                   listw=NYlistw, llprof=100)
nysarw_ll<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SAR",
                                listw=NYlistw, weights=POP8, llprof=100)


###################################################
### code chunk number 80: lat.Rnw:2670-2684
###################################################
ylim <- range(c(nysarw_ll$llprof$ll, nysar_ll$llprof$ll), na.rm=TRUE)
plot(nysarw_ll$llprof$lambda, nysarw_ll$llprof$ll, type="l", xlab=expression(lambda), ylab="log likelihood", ylim=ylim, lwd=2)
abline(v=nysarw_ll$lambda)
abline(h=nysarw_ll$LL)
lines(nysar_ll$llprof$lambda, nysar_ll$llprof$ll, lty=2, lwd=2)
abline(v=nysar_ll$lambda, lty=2)
abline(h=nysar_ll$LL, lty=2)
legend("bottom", legend=c("weighted SAR", "SAR"), lty=c(1,2), lwd=2, bty="n")


###################################################
### code chunk number 81: lat.Rnw:2711-2714
###################################################
nysmaw<-spatialreg::spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, family="SMA",
                             listw=NYlistw, weights=POP8)
summary(nysmaw)


###################################################
### code chunk number 82: lat.Rnw:2752-2754
###################################################
library(lmtest)
bptest(nylm)


###################################################
### code chunk number 83: lat.Rnw:2776-2779
###################################################
library(sandwich)
coeftest(nylm)
coeftest(nylm, vcov=vcovHC(nylm, type="HC4"))


###################################################
### code chunk number 84: lat.Rnw:2813-2818
###################################################
NYlistwW <- nb2listw(NY_nb, style = "W")
res <- lm.LMtests(nylm, listw=NYlistwW, test="all")
tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tres) <- c("Statistic", "df", "p-value")
printCoefmat(tres)


###################################################
### code chunk number 85: lat.Rnw:2901-2904
###################################################
nylag <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
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


