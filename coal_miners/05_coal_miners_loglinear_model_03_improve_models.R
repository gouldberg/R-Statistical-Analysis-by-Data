setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# add [B Age][W Age] term
# ------------------------------------------------------------------------------


# Both breathlessness and wheeze increase with age:  add [B Age][W Age] to baseline model
# (no 3-way interaction model)

cm.glm2 <- glm(Freq ~ B * W + (B + W) * Age, data = CM, family = poisson)


LRstats(cm.glm1, cm.glm2)




# ----------
car::Anova(cm.glm2)





# ------------------------------------------------------------------------------
# improve model:  odds ratio for the [BW] association to vary linearly with age.
# ------------------------------------------------------------------------------

# One way to improve the model using the glm() framework is to make use of Age as a quantitative variable
# and add a term to allow the odds ratio for the [BW] association to vary linearly with age.


# We construct the variable age using the midpoints of the Age intervals

CM$age <- rep(seq(22, 62, 5), each = 4)


CM$ageOR <- (CM$B == "B") * (CM$W == "W") * CM$age


head(CM)




# ----------

cm.glm3 <- update(cm.glm2, . ~ . + ageOR)


summary(cm.glm3)




LRstats(cm.glm0, cm.glm1, cm.glm2, cm.glm3)




# -->
# The model cm.glm3 with one more parameter, now fits reasonably well haveing residual G^2(7) = 6.8
# The likelihood ratio test of model cm.glm3 against cm.glm2, which assumes equal odds ratios over age,
# can be regarded as a test of the hypothesis of homogeneity of odds ratios, against the alternative that the
# [BW] association changes linearly with age.




# ----------

anova(cm.glm2, cm.glm3, test = "Chisq")





# -->
# glm() approach does not easily allow us to represent and test the substantively interesting hypotheses
# regarding how the prevalence of the binary responses, B and W, vary with Age.

# Also, it does not represent the odds ratio for the [BW] association directly, but only through the coding trick.
# It is difficult to interpret the coefficient for ageOR = -0.02613 in a substantively meaningful way,
# except that is shows that the odds ratio is decreasing.


