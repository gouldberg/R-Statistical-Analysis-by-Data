
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# model fit
# ------------------------------------------------------------------------------

library(lme4)


fm16.1mer <- lmer(visual ~ visual0 + time * treat.f + (1 | subject), data = armd)



print(fm16.1mer, corr = FALSE)




# ------------------------------------------------------------------------------
# correlation matrix for beta
# ------------------------------------------------------------------------------


vcov <- vcov(fm16.1mer)


corb <- cov2cor(vcov)


nms <- abbreviate(names(fixef(fm16.1mer)), 5)


rownames(corb) <- nms


corb



# ------------------------------------------------------------------------------
# variance - components estimates
# ------------------------------------------------------------------------------


VarCorr(fm16.1mer)


( sgma <- sigma(fm16.1mer) )




# ------------------------------------------------------------------------------
# The marginal variance-covariance matrix
# ------------------------------------------------------------------------------


A <- getME(fm16.1mer, "A")


I.n <- Diagonal(ncol(A))


V <- sgma ^ 2 * (I.n + crossprod(A))



# ----------
# Grouping factor
str(getME(fm16.1mer, "flist"))


V[3:6, 3:6]




# ------------------------------------------------------------------------------
# P-values for the marginal approach t-tests
# ------------------------------------------------------------------------------


coefs <- coef(summary(fm16.1mer))



# Denominator df
ddf <- c(631, 231, 631, 231, 631)



pT <- 2 * (1 - pt(abs(coefs[, "t value"]), ddf))



tTable <- cbind(coefs, ddf, pT)


printCoefmat(tTable, P.values = TRUE, has.Pvalue = TRUE)




# ------------------------------------------------------------------------------
# P-values for the sequential-approach F-tests
# ------------------------------------------------------------------------------


( dtaov <- anova(fm16.1mer) )



# ddf for intercept omitted
ddf1 <- ddf[-1]



# ----------
dtaov$'Pr(>F)' <- pf(dtaov$'F value', dtaov$npar, ddf1, lower.tail = FALSE)


dtaov$denDF <- ddf1


dtaov




# ------------------------------------------------------------------------------
# Refitting the model to the simulated data
# ------------------------------------------------------------------------------

SeedValue <- 17432157

set.seed(SeedValue)


merObject <- fm16.1mer



# simulated
simD1 <- simulate(merObject, nsim = 1000)



SimD1summ <- apply(simD1, 2,
                  function(y){
                    auxFit <- refit(merObject, y)
                    summ <- summary(auxFit)
#                    beta <- fixef(summ)
                    beta <- fixef(auxFit)
                    Sx <- getME(auxFit, "theta")
                    sgma <- sigma(auxFit)
                    list(beta = beta, ST = Sx, sigma = sgma)
                  })



# ----------
# matrices / vectors with estimates of beta, sqrt(d11/sigma^2), and sigma for all simulations

betaE <- sapply(SimD1summ, FUN = function(x) x$beta)


STe <- sapply(SimD1summ, FUN = function(x) x$ST)


sigmaE <- sapply(SimD1summ, FUN = function(x) x$sigma)




# ------------------------------------------------------------------------------
# Empirical means, quantiles, and p-values for fixed0effects coefficients
# ------------------------------------------------------------------------------


betaEm <- apply(betaE, 1, mean)



betaEq <- apply(betaE, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975)))



ptE <- apply(betaE, 1, FUN = function(x){
  prb <- mean(x > 0)
  2 * pmax(0.5 / ncol(betaE), pmin(prb, 1 - prb))
})



cbind(betaEm, t(betaEq), ptE)




# ------------------------------------------------------------------------------
# Empirical means nad quantiles for sqrt(d11) and sigma
# ------------------------------------------------------------------------------

d11E <- STe * sigmaE


rndE <- rbind(d11E, sigmaE)


rndEm <- rowMeans(rndE)


rndEq <- apply(rndE, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975)))


cbind(rndEm, t(rndEq))




# ------------------------------------------------------------------------------
# Density plots for the simulation-based estimates of the fixed-effects coefficients
# and variance-covariance parameters for model
# ------------------------------------------------------------------------------


names(sigmaE) <- names(STe) <- NULL


parSimD1 <- rbind(betaE, ST1 = STe, sigma = sigmaE)


parSimD1t <- data.frame(t(parSimD1), check.names = FALSE)


parSimD1s <- parSimD1t %>% dplyr::select(-'(Intercept)')



library(reshape)


lattice::densityplot(~ value | variable, data = melt(parSimD1s),
            scales = list(relation = "free"), plot.points = FALSE)



# -->
# density plots are relatively symmetric.
# They suggest, for instance, that CIs, based on the normal distribution approximation of the empirical distribution,
# might be adequate for construction of the interval estimates of the parameters.




# ------------------------------------------------------------------------------
# Test for random intercepts:  REML-based likelihood-ratio test
# ------------------------------------------------------------------------------


lm2.form <- visual ~ visual0 + time + treat.f + treat.f : time


vis.lm2 <- lm(lm2.form, data = armd)


( RLRTstat <- -2 * as.numeric(logLik(vis.lm2, REML = TRUE) - logLik(fm16.1mer)) )


0.5 * pchisq(RLRTstat, 1, lower.tail = FALSE)




# ------------------------------------------------------------------------------
# Test for random intercepts:  Using the function exactRLRT() to simulate the null distribution
# ------------------------------------------------------------------------------


library(RLRsim)

exactRLRT(fm16.1mer)





# ------------------------------------------------------------------------------
# Test for random intercepts:  Using the simulate.mer() method to obtain the empirical p-value
# ------------------------------------------------------------------------------


lm2sim <- simulate(vis.lm2, nsim = 100)


RLRTstatSim <- apply(lm2sim, 2,
                     function(y){
                       dfAux <- within(armd, visual <- y)
                       lm0 <- lm(formula(vis.lm2), data = dfAux)
                       llik0 <- as.numeric(logLik(lm0, REML = TRUE))
                       llikA <- as.numeric(logLik(refit(fm16.1mer, y)))
                       RLRTstat <- -2 * (llik0 - llikA)
                     })


mean(RLRTstat <= RLRTstatSim)




# ------------------------------------------------------------------------------
# Test for random slopes:  fitted using the function lmer()
# Fitting the model and extracting basic information
# ------------------------------------------------------------------------------


fm16.2mer <- lmer(visual ~ visual0 + time + treat.f + treat.f : time + 
                    (1 | subject) + (0 + time | subject), data = armd)



summ <- summary(fm16.2mer)


coef(summ)



unlist(VarCorr(fm16.2mer))


sigma(fm16.2mer)




# ------------------------------------------------------------------------------
# Test for random slopes:  fitted using the function lmer()
# Likelihood-ratio test for the treat.f : time interaction
# ------------------------------------------------------------------------------

fm16.2aux <- update(fm16.2mer, . ~ . - treat.f : time)


anova(fm16.2aux, fm16.2mer)




# ------------------------------------------------------------------------------
# The REML-based likelihood-ratio test for no random slopes in model
# ------------------------------------------------------------------------------

RML0 <- logLik(fm16.1mer)


RMLa <- logLik(fm16.2mer)


(RLRTstat <- -2 * as.numeric(RML0 - RMLa))  


.5 * pchisq(RLRTstat, 1, lower.tail = FALSE) +    # \zx{p}-value
  .5 * pchisq(RLRTstat, 2, lower.tail = FALSE)     




# ----------
require(RLRsim)

mAux  <- lmer(visual ~               # Auxiliary model with ...
                visual0 + time + treat.f + treat.f:time +  
                (0 + time| subject),        # ... random slopes only. 
              data = armd)          


exactRLRT(m = mAux,                  # The auxiliary model
          m0= fm16.1mer,             # M16.1 (null)
          mA= fm16.2mer)          


