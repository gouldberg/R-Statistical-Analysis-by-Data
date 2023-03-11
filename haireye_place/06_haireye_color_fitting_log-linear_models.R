setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



# ------------------------------------------------------------------------------
# Fitting models
#  - complete (mutual) independence:  all joint probabilities are products of the 1-way marginal probabilities:   [Hair][Eye][Sex]  (~ Hair + Eye + Sex)
#  - joint independence:              Sex is jointly independent of Hair and Eye:             [HairEye][Sex]  (~ Hair * Eye + Sex)
#  - conditional independence:        Hair and Eye are conditionally independent givne Sex:   [HairSex][EyeSex]  (~ Hair * Sex + Eye * Sex = ~ (Hair + Eye) * Sex)
#  - No 3-way interaction:            no pair is marginally or conditionally independent:     [HairEye][HairSex][EyeSex]  (~ Hair * Eye + Hair * Sex + Eye * Sex)
# ------------------------------------------------------------------------------
# Note that the pattern of residuals here is similar to that in the tow-way display. (collapsed over sex)
# Still the model [HairSex][EyeSex] fit very poorly

#  --> The pattern of residuals in the mosaic will suggest associations to be added to an adequate explanatory model.
# As the model achieves better fit to the data, the degree of shading decreases, so we may think of the process of model fitting as "clearning the mosaic"

abbrev <- list(abbreviate = c(FALSE, FALSE, 1))
vcd::mosaic(HEC, expected = ~ Hair + Eye + Sex, labeling_args = abbrev, main = "Model: ~ Hair + Eye + Sex")
vcd::mosaic(HEC, expected = ~ Hair * Sex + Eye * Sex, labeling_args = abbrev, main="Model: ~ Hair*Sex + Eye*Sex")



# ----------
# three types of independence
mod1 <- MASS::loglm(~ Hair + Eye + Sex, data = HEC)       # mutual
mod2 <- MASS::loglm(~ Hair * Eye + Sex, data = HEC)       # joint
mod3 <- MASS::loglm(~ Hair * Sex + Eye * Sex, data = HEC) # conditional
vcdExtra::LRstats(mod1, mod2, mod3)



# ----------
# Alternatively you can get the Pearson and likelihood ratio (LR) tests for a given model using anova()
anova(mod1)



# ----------
# or compare a set of models using LR tests on the difference in LR X^2 from one model to the next, when a list of models is supplied to anova()
anova(mod1, mod2, mod3, test = "chisq")



# ------------------------------------------------------------------------------
# Fitting models
# decomposition of the G^2 for mutual independence (total) as the sum of marginal and joint independence
# ------------------------------------------------------------------------------
# Mutual = Marginal + Joint
vcd::mosaic(HEC, expected = ~ Hair + Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Mutual")

vcd::mosaic(~ Hair + Eye, data = HEC, shade = TRUE, legend = FALSE, main = "Marginal")

vcd::mosaic(HEC, expected = ~ Hair * Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Joint")



# ----------
# G^2 (likelihood ratio) of mutual independence = G^2 of marginal independence + G^2 of joint independence 
mod1$lrt == mod2$lrt + mod3$lrt

