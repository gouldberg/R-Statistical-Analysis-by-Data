setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

linmod2 <- lm(involact ~ -1 + race + fire + age + theft, data = chredlin)

linmod3 <- lm(involact ~ race + fire + age, data = chredlin)

linmod4 <- lm(involact ~ race + fire + theft + log(income) * age, data = chredlin)


library(mgcv)
twmod <- gam(involact ~ race + fire + theft + age + log(income), family = tw(link = "log"), data = chredlin)




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


mynames <- paste0("M", as.character(1:4))



# second.ord = TRUE/FALSE:  AICc/AIC is computed

myaicc <- aictab(cand.set = list(linmod, linmod2, linmod3, linmod4),
                modnames = mynames, sort = TRUE, second.ord = TRUE)


as.data.frame(myaicc)



# -->
# "AICcwt":  best M4 (linmod4) is 0.64 in AICcwt




# ------------------------------------------------------------------------------
# Multimodel inference:  confidence sets
#   - It is not always the case that a single model will have such a large (0.9 or 0.95) probability.
#     In such cases, it is convenient to form a confidence set of the models whose probabilities sum to 0.90 to 0.95
# ------------------------------------------------------------------------------

confset(cand.set = list(linmod, linmod2, linmod3, linmod4), modnames = mynames, level = 0.75)



# -->
# M2, M4 constitutes sum of probability up to 0.9



# ------------------------------------------------------------------------------
# Multimodel inference:  Evidence Ratio
# ------------------------------------------------------------------------------

# Evidence ratio for the hth model = W(max) / W(h)


# single evidence ratio
evidence(myaicc)



# -->
# M4 is the best approximating model is about 2.61 to 1 over M2




# ----------
# all evidence ratio

# exclude logLik
myaicc2 <- as.data.frame(myaicc)[,-7]

myaicc2$Eratio <- round(max(myaicc2$AICcWt) / myaicc2$AICcWt, 3)



data.frame(Model = myaicc2$Modnames, round(myaicc2[,-1], 3))


