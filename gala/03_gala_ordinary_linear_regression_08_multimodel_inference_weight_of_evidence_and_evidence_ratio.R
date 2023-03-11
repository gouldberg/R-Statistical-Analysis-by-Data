]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ------------------------------------------------------------------------------
# models
# ------------------------------------------------------------------------------

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)

lmod_final <- lm(Species ~ Elevation + Adjacent, data = gala)

lmod_r_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala[-16,])



# ----------
# not excluding Isabela
lmod_r2_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala)



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


mynames <- paste0("M", as.character(1:6))



# second.ord = TRUE/FALSE:  AICc/AIC is computed

myaicc <- aictab(cand.set = list(lmod, lmods, lmods2, lmod4, lmod_final, lmod_r2_final),
                modnames = mynames, sort = TRUE, second.ord = TRUE)


as.data.frame(myaicc)



# -->
# "AICcwt":  best M5 (lmod_final) is 0.638 in AICcwt




# ------------------------------------------------------------------------------
# Multimodel inference:  confidence sets
#   - It is not always the case that a single model will have such a large (0.9 or 0.95) probability.
#     In such cases, it is convenient to form a confidence set of the models whose probabilities sum to 0.90 to 0.95
# ------------------------------------------------------------------------------

confset(cand.set = list(lmod, lmods, lmods2, lmod4, lmod_final, lmod_r2_final), modnames = mynames, level = 0.75)



# -->
# M5 and M6 constitutes sum of probability up to 0.88



# ------------------------------------------------------------------------------
# Multimodel inference:  Evidence Ratio
# ------------------------------------------------------------------------------

# Evidence ratio for the hth model = W(max) / W(h)


# single evidence ratio
evidence(myaicc)



# -->
# M5 is the best approximating model is about 2.61 to 1 over M6




# ----------
# all evidence ratio

# exclude logLik
myaicc2 <- as.data.frame(myaicc)[,-7]

myaicc2$Eratio <- round(max(myaicc2$AICcWt) / myaicc2$AICcWt, 3)



data.frame(Model = myaicc2$Modnames, round(myaicc2[,-1], 3))

