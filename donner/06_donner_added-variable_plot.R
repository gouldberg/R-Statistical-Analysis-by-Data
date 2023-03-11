setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))

fam <- data$family
levels(fam)[c(3,4,6,7,9)] <- "Others"

fam <- factor(fam, levels(fam)[c(1, 2, 4:6, 3)])
data$family <- fam



# ----------
donner.mod1 <- glm(survived ~ age + sex, data = data, family = binomial)
donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Diagnostic plot:  Added-variable plots (partial-regression plots)
#  - Marginal plots of response variable against the predictor variables can conceal or misrepresent the relationships in a model
#    including several predictors together due to correlations or associations among the predictor.
#    Added-variable plots solve this problem by plotting the residuals which are less discrete than the marginal responses in Y
#  - Sets of two (or more) observations can have joint influence, which greatly exceeds their individual influential.
#    Similarly, the influence of one discrepant point can be offset by another influential point in an opposite direction, a phenomenon called masking.
#  - Added-variable plots, showing the partial regression for one predictor controlling or adjusting for all others, can make such cases visually apparent.
# ------------------------------------------------------------------------------
# Those who survived are shown in blue; those who died in red. Men are plotted with filled circles; women with filled triangles.
col <- ifelse(data$survived == "yes", "blue", "red")
pch <- ifelse(data$sex == "Male", 16, 17)
car::avPlots(donner.mod1, id = TRUE, col = col, pch = pch, col.lines = "darkgreen")



car::avPlots(donner.mod3, id = TRUE, col = col, pch = pch, col.lines = "darkgreen")

