setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
#  - frequencies of hair color, eye color, and sex from 592 students in a statistical course (Snee's 1974 sample)
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

dim(HairEyeColor)
str(HairEyeColor)

HairEyeColor


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(HairEyeColor, 1:2) )



# ------------------------------------------------------------------------------
# 3-way and larger tables:  classical test of independence
#
# the independence of Hair and Eye:  the model [Hair][Eye]
# ------------------------------------------------------------------------------
HE <- MASS::loglm(~ Hair + Eye, data = haireye)


# The output includes both the chi-square statistic and the deviance test statistics, both significant, indicating strong lack of fit.
HE



# ------------------------------------------------------------------------------
# 3-way and larger tables:  classical test of independence
#
# Hair and Eye are jointly independent of Sex:  the model [HairEye][Sex]
# ------------------------------------------------------------------------------
# From mosaic plot, the hypothesis is that Hair and Eye are jointly independent of Sex.
# To test formally, we fit corresponding model [HairEye][Sex]
#  --> non-significant Pearson chi-square (15) = 19.567, p = 0.189
HE_S <- MASS::loglm(~ Hair * Eye + Sex, data = HairEyeColor)
HE_S


# residuals
residuals(HE_S, type = "pearson")


# Show residuals on mosaic plot
# vcd::mosaic(expected = ~, labeling = labeling_residuals) shows residuals from loglm model on mosaic plot
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
vcd::mosaic(HEC, expected = ~ Hair * Eye + Sex, labeling = labeling_residuals, digits = 2, rot_labels = c(right = -45))


# --> Although non-significant, the two largest residuals highlighted in the plot account for nearly half (-2.15^2 + 2.03^2 = 8.74) of the lack of fit,
# and so are worthy of attention here.



# ------------------------------------------------------------------------------
# 3-way and larger tables:  classical test of independence
#
# mutual independence [Hair][Eye][Sex]
# joint-independence model [HairSex][EyeSex]
# ------------------------------------------------------------------------------
# Note that the pattern of residuals here is similar to that in the tow-way display. (collapsed over sex)
# Still the model [HairSex][EyeSex] fit very poorly
abbrev <- list(abbreviate = c(FALSE, FALSE, 1))
vcd::mosaic(HEC, expected = ~ Hair + Eye + Sex, labeling_args = abbrev, main = "Model: ~ Hair + Eye + Sex")
vcd::mosaic(HEC, expected = ~ Hair * Sex + Eye * Sex, labeling_args = abbrev, main="Model: ~ Hair*Sex + Eye*Sex")



# ----------
# three types of independence
mod1 <- MASS::loglm(~ Hair + Eye + Sex, data = HEC)       # mutual
mod2 <- MASS::loglm(~ Hair * Sex + Eye * Sex, data = HEC) # conditional
mod3 <- MASS::loglm(~ Hair * Eye + Sex, data = HEC)       # joint
vcdExtra::LRstats(mod1, mod2, mod3)


# Alternatively you can get the Pearson and likelihood ratio (LR) tests for a given model using anova()
anova(mod1)
anova(mod1, mod2, mod3, test = "chisq")


