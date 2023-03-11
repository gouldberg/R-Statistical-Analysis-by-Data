# setwd("//media//kswada//MyFiles//R//mpls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//mpls")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This examples are based on "Longitudinal Data Analaysis for the Behavioral Sciences Using R"


# ------------------------------------------------------------------------------
# data:  MPLS
#   - Sample data from Minneapolis School District (MPLS)
# ------------------------------------------------------------------------------

MPLS.LS <- read.table("MPLS.LS.txt", header = T)


str(MPLS.LS)


dim(MPLS.LS)


car::some(MPLS.LS)



# ------------------------------------------------------------------------------
# Compute AIC and compare models
# ------------------------------------------------------------------------------

MPLS.LS$grade5 <- MPLS.LS$grade - 5

MPLS.LS <- MPLS.LS %>% mutate(eth2 = ifelse(eth == "Whi", "W", "Other"))



# ----------
# estimate models
# anchoring of the intercept at 5th grade

library(lme4)

model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)

model.2 <- lmer(read ~ grade5 + eth + (grade5 | subid), MPLS.LS, REML = FALSE)

model.3 <- lmer(read ~ grade5 + eth + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)




# ----------
# Compute AIC

library(AICcmodavg)

mynames <- paste("M", as.character(1:3), sep = "")

myaic <- aictab(cand.set = list(model.1, model.2, model.3),
                modnames = mynames, sort = FALSE, second.ord = FALSE)


as.data.frame(myaic)[,1:3]



# ------------------------------------------------------------------------------
# Compute AICc (AIC Corrected) and compare models
#   - When the sample size is not large, a bias-adjusted from of the AIC should be used, the AIC-corrected or AICc
#     AICc = ACI + 2 * K * (K + 1) / ( sum(n(i)) - K - 1 )
#     (for the ordinaly linear regression. the summation is replaced with N)
#   - The AICc should be regularly used rather than the AIC, to cover both small- and large-sample situations.
# ------------------------------------------------------------------------------

# second.ord = TRUE is default and this computes AICc
myaicc <- aictab(cand.set = list(model.1, model.2, model.3),
                modnames = mynames, sort = FALSE, second.ord = TRUE)


as.data.frame(myaicc)[,1:3]



# ------------------------------------------------------------------------------
# AICc and Effect Size
#   - Since AICc is a sample statistic, it is influenced by sampling fluctuations and is not a completely reliable indicator of the best approximating model.
#     Two models with similar values are almost equally plausible in regard to the best approximating model.
# ------------------------------------------------------------------------------
#   - Delta:
#        - Difference between each AICc and the smallest AICc of the candidate models
#        - delta = 4:  "strong" difference
#        - delta = 8:  "very strong" difference
# ------------------------------------------------------------------------------
#   - Weight of Evidence
#        - probability scaling of delta = exp(-0.5 * delta) / sum( exp(-0.5 * delta) )
#        - Given the data, the ser of models, and the unknowable true model, weight of evidence indicates the probability that the model is the best approximating model.
#        - The weight of evidence can be used as a representation of the extent of model evaluation uncertainty.
#          This refers to the uncertainty that a model in question is, in fact, the best approximating model.
#          The uncertainty arises from the reality that models are evaluated based on a single sample.
#        - The large probability speaks only to the relative fit of the model, not the absolute fit.
# ------------------------------------------------------------------------------

print(myaicc)

# print(myaicc, LL = FALSE)



# ------------------------------------------------------------------------------
# Confidence set for the best model
# ------------------------------------------------------------------------------

# With only 3 models, the confidence set is easy to dtermine by inspecting the output.
# However, with additional models. it is convenient to use the confset() function in AICcmodavg

# The default confidence is 95%
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames)


# 75% confidence set
confset(cand.set = list(model.1, model.2, model.3), modnames = mynames, level = 0.75)




# ------------------------------------------------------------------------------
# AICc and Effect Size
# ------------------------------------------------------------------------------
#   - Evidence Ratio:
#        - The difference between the best-fitting model and a worse-fitting model in terms of odds
#          = max(weight of evidence) / weight of evidence of the model
# ------------------------------------------------------------------------------

# The default of the function is to return the evidence ratio for the best-fitting model and the second best-fitting model
evidence(myaicc)


# Compare best-fitting model and "M1"
evidence(myaicc, model.low = "M1")



# ----------
myaicc2 <- myaicc %>% arrange(AICc) %>% mutate(Cum.Wt = cumsum(AICcWt), Eratio = max(AICcWt) / AICcWt)

print(myaicc2, digits = 3)



