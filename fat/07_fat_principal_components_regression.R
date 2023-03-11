setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)


# take only body circumference
names(fat)

cfat <- fat[,9:18]




# ------------------------------------------------------------------------------
# Principal components regression for meaningful explanation
#   - principal components are orthogonal, meaning we can now interpret regression result without collinearity worries.
# ------------------------------------------------------------------------------

# linear regression by only body circumference variables
lmoda <- lm(fat$brozek ~ ., data = cfat)

summary(lmoda)


# -->
# It is difficult to say much about which factors might influence body fat percentage because there are clear indications of collinearity.
# The signs of the coefficients and their significance vary in a less than credible way.
# Why would abdomen circumference have a positive effect while hip circumference has negative effect ?


# ----------
# principal components regression

prfatc <- prcomp(cfat, scale = TRUE)

summary(prfatc)


lmodpcr <- lm(fat$brozek ~ prfatc$x[,1:2])

summary(lmodpcr)



# -->
# The first PC can be viewed as a measure of overall size. This is associated with higher body fat.
# The second PC shows a negative association, meaning that men who carry relatively more of their substance in their extremities tend to be leaner.
# These would tend to be men who are more muscular so this reuslt accords with that one might expect.

# But principal components regression lost some explanatory power



# ------------------------------------------------------------------------------
# linear regression only by important variables
#   - One objection to principal components regression is that the two PCs still use all ten predictors.
#     To answer this, one idea is to take a few representative predictors based on the largest coefficients seen in the PCs.
#     We need to scale the predictors.
# ------------------------------------------------------------------------------

lmodr <- lm(fat$brozek ~ scale(abdom) + I(scale(ankle) - scale(abdom)), data = cfat)

summary(lmodr)



# -->
# we have a simple model that fits almost as well as the ten-predictor model.


# But usually the predictors would need to have the same units. So if we had used the age and weight variables found in the fat data example,
# it would have been far more difficult to interpret the linear combinations.
# These requirements restrict the utility of PCR for explanatory purposes.



# ------------------------------------------------------------------------------
# Principal Components Regression by pls package
# ------------------------------------------------------------------------------

library(pls)

set.seed(123)

pcrmod <- pcr(brozek ~ ., data = fat, ncomp = 10, validation = "CV")

summary(pcrmod)



# ----------
# crossvalidated estimates of the RMSE
# The minimum occurs at around 4 components
pcrCV <- RMSEP(pcrmod, estimate = "CV")

plot(pcrCV, main ="")



# ----------
# let's use the first 4 PCs to predict the response
library(ModelMetrics)

rmse(predict(pcrmod, ncomp = 4), fat$brozek)



# ----------
# pot the coefficients from the 4-component fit
coefplot(pcrmod, ncomp = 4, xlab = "", main = "")
