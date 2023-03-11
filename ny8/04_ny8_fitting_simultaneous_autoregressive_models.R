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


NY8$lmresid <- residuals(nylm)



# ----------
# The Breusch-Pagan test results indicate the presence of heteroskedasticity
# This might suggest the need to adjust the estimated coefficient standard errors using a variance-covariance matrix taking heteroskedasticity into account.
library(lmtest)

bptest(nylm)



# ----------
# But there are only minor changes in the standard errors, and they do not affect our inferences.
library(sandwich)

coeftest(nylm)

coeftest(nylm, vcov = vcovHC(nylm, type = "HC4"))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
NYlistw <- nb2listw(NY_nb, style = "B")

lm.morantest(nylm, NYlistw)


NYlistwW <- nb2listw(NY_nb, style = "W")
aple(residuals(nylm), listw=NYlistwW)
spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)$lambda



# ------------------------------------------------------------------------------
# SAR (Simultaneous Autoregressive Models)
#
#   - The SAR specification uses a regression on the values from the other areas to account for the spatial dependence.
#     The error terms are modelled so that they are depend on each other.
# ------------------------------------------------------------------------------
NYlistw <- nb2listw(NY_nb, style = "B")

nysar <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, listw=NYlistw)


summary(nysar)


# -->
# There is significant spatial correlation in the residuals because the estimated value of lambda (spatial autocorrelation parameter)
# is 0.0405 and the p-value of the likelihood ratio test is 0.022.
# In the likelihood ratio test we compare the model with no spatial autocorrelation (lambda = 0) to the one which allows for it.

# The proximity to a TCE seems not to be significant, although its p-value is close to being significant at the 95% level.
# It would be advisable not to discard a possible association and to conduct further research on this.

# The other two covariates are significant, suggesting that census tracts with larger percentages of older people and with
# lower percentages of house owners have higher transformed incidence rates.



# ----------
nylam1 <- c(nysar$lambda)
nylam2 <- c(LR1.spautolm(nysar)$p.value)


NY8$sar_trend <- nysar$fit$signal_trend
NY8$sar_stochastic <- nysar$fit$signal_stochastic

rds <- colorRampPalette(brewer.pal(8, "RdBu"))
tr_at <- seq(-1, 1.3, length.out=21)
tr_rds <- rds(sum(tr_at >= 0)*2)[-(1:(sum(tr_at >= 0)-sum(tr_at < 0)))]
tr_pl <- spplot(NY8, c("sar_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_at <- seq(-0.16, 0.39, length.out=21)
st_rds <- rds(sum(st_at >= 0)*2)[-(1:(sum(st_at >= 0)-sum(st_at < 0)))]

st_pl <- spplot(NY8, c("sar_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))
plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)



# -->
# left:  trend component of SAR model fitted values
# right:  spatial stochastic component of SAR model fitted values



# ------------------------------------------------------------------------------
# SAR (Simultaneous Autoregressive Models)
# taking into account for the heterogeneous distribution of the population by tracts
# beyond the correction introduced in transforming incidence proportions.
# ------------------------------------------------------------------------------
nylmw <- lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, weights=POP8)

summary(nylmw)



# -->
# We can see that the TCE exposure variable has become significant with the expected sign,
# indicating that tracts closer to the TCE sites have slightly higher transformed incidence



NY8$lmwresid <- residuals(nylmw)



# ----------
library(RColorBrewer)
gry <- c(rev(brewer.pal(6, "Reds")[1:4]), colorRampPalette(brewer.pal(5, "Blues"))(9))
TCEpts <- list("sp.points", TCE, pch=16, col="grey5")
spplot(NY8, c("lmresid", "lmwresid"), sp.layout=list(TCEpts), col.regions=gry, col="transparent", lwd=0.5, at=seq(-2,4.5,0.5))


# -->
# left: residuals from the linear model of transformed incidence proportions
# right: residuals from the weighted linear model of transformed incidence proportions
# TCE site locations shown for comparative purposes.



# ----------
# The Moral tests for regression residuals can also be used with a weighted linear model object.
# The results are interesting, suggesting that the misspecification detected by Moran's I is in fact related to heteroskedasticity more than
# to spatial autocorrelation.
lm.morantest(nylmw, NYlistw)



# ----------
# We can check this for the SAR model too.
# The coefficients of the covariates change slightly in the new model, and all the coeffs p-values drop substantially.
# In this eighted SAR fit, proximity to a TCE site becomes significant.

nysarw <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, listw=NYlistw, weights=POP8)
# nysarw <- spatialreg::spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, listw=NYlistw, weights=POP8)

summary(nysarw)



# ----------
# However, there are no traces of spatial autocorrelation left after adjusting for the heterogeneous size of the population
# This suggests that the spatial variation in population between tracts is responsible for the observed residual spatial correlation
# after adjusting for covariates.
NY8$sarw_trend <- nysarw$fit$signal_trend
NY8$sarw_stochastic <- nysarw$fit$signal_stochastic

tr_pl <- spplot(NY8, c("sarw_trend"), at=tr_at, col="transparent", col.regions=tr_rds, main=list(label="Trend", cex=0.8))
st_pl <- spplot(NY8, c("sarw_stochastic"), at=st_at, col="transparent", col.regions=st_rds, main=list(label="Stochastic", cex=0.8))

plot(tr_pl, split=c(1,1,2,1), more=TRUE)
plot(st_pl, split=c(2,1,2,1), more=FALSE)



# ----------
# The weighted model provides a better fitting since its AIC is considerable lower
AIC(nylm)
AIC(nysar)
AIC(nylmw)
AIC(nysarw)

