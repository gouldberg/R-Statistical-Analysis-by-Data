setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
# ------------------------------------------------------------------------------

getHdata(prostate)

str(prostate)

car::some(prostate)


# ----------
# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate
str(data)


data$cvd <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular")



# ------------------------------------------------------------------------------
# conversion
# ------------------------------------------------------------------------------

# combining an infrequent category with the next category, and dichotomizing ekg

levels(data$ekg)[levels(data$ekg) %in% c("old MI", "recent MI")] <- "MI"

data$ekg.norm <- 1 * (data$ekg %in% c("normal", "benign"))

levels(data$ekg) <- abbreviate(levels(data$ekg))

data$pfn <- as.numeric(data$pf)

levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]

data$rxn <- as.numeric(data$rx)



# ------------------------------------------------------------------------------
# Redundancy Analysis
#   - redun() implements the folowing redundancy checking algorithm
#       1. Expand each continuous predictor into restricted cubic spline basis functions. Expand categorical predictors into dummy variables.
#       2. Use OLS to predict each predictor with all component terms of all remaining predictors.
#          Wen the predictor is expanded into multiple terms, use the first canonical variate.
#       3. Remove the predictor that can be predicted from the remaining set with the highest adjusted or regular R^2
#       4. Predict all remaining predictors from their complement.
#       5. Continue in like fashion
#             untill no variable still in the list of predictors can be predicted with an R^2 or adjusted R^2 greater than a specified threshold or
#             until dropping the variable with the highest R^2 would cause a variable that was dropped earlier to no longer be predicted at the threshold from the now smaller list of predictors.
# ------------------------------------------------------------------------------

# args(redun)

r2_cutoff <- 0.3


# Note all incomplete cases are deleted (inefficient)
( res <- redun(~ stage + I(rxn) + age + wt + I(pfn) + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, r2 = r2_cutoff, type="adjusted", data = data) )


res$In
res$Out


# -->
# redundant variables:  stage, sbp, bm, sg
# stage can be predicted with R^2 = 0.658 (0.655) from other 13 variables,
# but only with R^2 = 0.493 (0.488) after deletion of 3 variables lated declared to be redundant.



# ------------------------------------------------------------------------------
# For reference:  VIF from logistic regression
# ------------------------------------------------------------------------------
lmod <- glm(cvd ~ stage + I(rxn) + age + wt + I(pfn) + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, data = data, family = binomial(link=logit))

summary(lmod)

sort(vif(lmod), decreasing = TRUE)

