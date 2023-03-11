# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\silvia")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  silvia
# ------------------------------------------------------------------------------

dat <- read.csv("silvia_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$TM <- as.factor(dat$TM)


dat$DEAL <- as.factor(dat$DEAL)



# ----------
# price down
dat <- dat %>% mutate(pd = P - NP)


# KM == 0
dat <- dat %>% mutate(km_z = KM == 0)




# ----------
mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)

mod1 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z, data = dat)

mod2 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z + KM : TM, data = dat)

mod3 <- lm(P ~ NP + KM + TM + Yr + km_z, data = dat)

mod4 <- lm(P ~ NP + KM + TM + I(Yr^2) + I((24 - CT)^2) + km_z, data = dat)

mod_step <- lm(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)


mod_obj <- mod3

mod_obj <- mod_step




# ------------------------------------------------------------------------------
# Multimodel inference:  AICc and Weight of Evidence
#   -  weight of evidence of the hth model: W(h) = exp(-0.5 * delta(h)) / sum( exp(-0.5 * delta) )
#      Given the data, the set of models, and the uknowable true model, W(h) indicates the probability
#      that model h is the best approximating model
#      It has been suggested that W(h) = 0.90 and 0.95 are reasonable benchamrks
#      Farily confident the best-fitting model is, in fact, the true best approximating model (not true model)
#      if its probability of being so is at least 0.90
# ------------------------------------------------------------------------------


library(AICcmodavg)


mynames <- paste0("M", as.character(1:5))



# second.ord = TRUE/FALSE:  AICc/AIC is computed
# response variable should be same, the loglog models are excluded

myaicc <- aictab(cand.set = list(mod1, mod2, mod3, mod4, mod_step),
                modnames = mynames, sort = TRUE, second.ord = TRUE)


as.data.frame(myaicc)



# -->
# "AICcwt":  best M5 (mod_step) is best 0.97 in AICcwt




# ------------------------------------------------------------------------------
# Multimodel inference:  confidence sets
#   - It is not always the case that a single model will have such a large (0.9 or 0.95) probability.
#     In such cases, it is convenient to form a confidence set of the models whose probabilities sum to 0.90 to 0.95
# ------------------------------------------------------------------------------

confset(cand.set = list(mod1, mod2, mod3, mod4, mod_step), modnames = mynames, level = 0.75)



# -->
# M5 constitutes sum of probability up to 0.97



# ------------------------------------------------------------------------------
# Multimodel inference:  Evidence Ratio
# ------------------------------------------------------------------------------

# Evidence ratio for the hth model = W(max) / W(h)


# single evidence ratio
evidence(myaicc)



# -->
# M5 is the best approximating model is about 72.73 to 1 over M3




# ----------
# all evidence ratio

# exclude logLik
myaicc2 <- as.data.frame(myaicc)[,-7]

myaicc2$Eratio <- round(max(myaicc2$AICcWt) / myaicc2$AICcWt, 3)



data.frame(Model = myaicc2$Modnames, round(myaicc2[,-1], 3))




