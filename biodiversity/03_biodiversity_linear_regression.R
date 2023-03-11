# setwd("//media//kswada//MyFiles//R//biodiversity")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\biodiversity")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Data:  Biodiversity
# ------------------------------------------------------------------------------

Biodiv <- read.table(file = "Biodiversity.txt", header = TRUE)

str(Biodiv)



# ----------
Biodiv$fTreatment <- factor(Biodiv$Treatment)

Biodiv$fNutrient <- factor(Biodiv$Nutrient)



# ------------------------------------------------------------------------------
# Linear Regression as starting point
# ------------------------------------------------------------------------------

# Initial linear regression analysis, using biomass, enrichment and nutrient, with all the two-way interactions,
# and the three-way interaction as explanatory variables
M0 <- lm(Concentration ~ Biomass * fTreatment * fNutrient, data = Biodiv)

summary(M0)



# ----------
anova(M0)



# ------------------------------------------------------------------------------
# simple model validation
# ------------------------------------------------------------------------------

op <- par(mfrow = c(1,1))

plot(M0, which = c(1), col = 1, add.smooth = T, caption = "")

par(op)



# -->
# Serious violation of homogeneity.
 
