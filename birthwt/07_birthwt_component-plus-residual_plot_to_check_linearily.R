setwd("//media//kswada//MyFiles//R//birthwt")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birthwt
#   - Data on 189 babies born at Baystate Medical Center, Springfield, MA during 1986.
#   - The quantitative response is bwt (birth weight in grams), and this is also recoded as low, a binary variable corresponding to bwt < 2500 (2.5 kg).
# ------------------------------------------------------------------------------
data("birthwt", package = "MASS")

data <- birthwt

dim(data)
str(data)


# ----------
Hmisc::describe(data)



# ----------
data$race <- factor(data$race, labels = c("white", "black", "other"))
data$ptd <- factor(data$ptl > 0)  # premature labors
data$ftv <- factor(data$ftv)  # physician visits
levels(data$ftv)[-(1:2)] <- "2+"
data$smoke <- factor(data$smoke > 0)
data$ht <- factor(data$ht > 0)
data$ui <- factor(data$ui > 0)

data$low <- factor(data$low, levels = c(0, 1))
data$bwt <- NULL
data$ptl <- NULL


levels(data$smoke) <- c("-", "smoke")
levels(data$ht) <- c("-", "ht")
# levels(data$ui) <- c("-", "ui")
levels(data$ptd) <- c("-", "ptd")


btw.final <- glm(low ~ age + race + smoke + ht + ptd, data = data, family = binomial)
btw.final2 <- glm(low ~ lwt + ht + ptd, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Diagnostic plot:  Component-plus-residual plots (partial residual plot)
#  - Designed to show whether a given quantiative predictor x included linearly in the model, actually shows a nonlinear relation, requiring transformation.
#  - Most useful when there are several quantitative predictors, so that it is convenient and sensible to examine their relationships individually.
#  - The essential idea is to move the linear term for x back into the residual, by calculating the partial residuals.
# ------------------------------------------------------------------------------
# The dashed red line shows the slope of age in the full model, the smoothed green curve shows a loess fit with span = 0.5.
# The smoothed loess curve in this plot closely resembles the trend we saw in the conditional plot for age by sex,
# suggesting the need to include a nonlinear term for age.
# The points identified in this plot, by default, are those with either the most extreme x values (giving them high leverage)
# or the largest absolute Pearson residuals in the full model.

# BUT please note that crPlots() and crPlot() does not work for those with interaction terms
car::crPlots(btw.final, ~ age, smooth = list(span = 0.5), id = TRUE)

car::crPlots(btw.final2, ~ lwt, smooth = list(span = 0.5), id = TRUE)





