setwd("//media//kswada//MyFiles//R//wcgs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wcgs
# ------------------------------------------------------------------------------

data("wcgs", package = "faraway")

str(wcgs)



# ------------------------------------------------------------------------------
# raw, deviance and pearson residuals vs. linear predictors and predicted probabilities (ratio)
# ------------------------------------------------------------------------------

linpred <- predict(lmod)
predprob <- predict(lmod, type = "response")


# raw residuals
( rawres <- residuals(lmod, type = "response") )


# standard deviance residuals
devres <- rstandard(lmod, type = "deviance")


# standard pearson residuals
pearres <- rstandard(lmod, type = "pearson")



# ----------
# We have chosen to plot the linear predictor rather than the predicted probability on the horizontal axis.
# The former provides a better spacing of the points in this direction

par(mfrow=c(2,3))
plot(rawres ~ linpred, xlab = "linpred", ylab = "raw resid")
plot(devres ~ linpred, xlab = "linpred", ylab = "st. deviance resid")
plot(pearres ~ linpred, xlab = "linpred", ylab = "st. pearson resid")
plot(rawres ~ predprob, xlab = "predprob", ylab = "raw resid")
plot(devres ~ predprob, xlab = "predprob", ylab = "st. deviance resid")
plot(pearres ~ predprob, xlab = "predprob", ylab = "st. pearson resid")



# ----------
# This is standardized deviance residuals
par(mfrow=c(1,2))
plot(devres ~ linpred, xlab = "linpred", ylab = "st. deviance resid")
plot(lmod, 1)



# ------------------------------------------------------------------------------
# Residuals plot by grouping the residuals into bins
# ------------------------------------------------------------------------------

# We choose 100 bins so that we have roughly 30 observations per bin

library(dplyr)

wcgs <- mutate(wcgs, residuals = residuals(lmod), linpred = predict(lmod))


# The cut function divides a variable into a factor with levels defined by breakpoints given by breaks
( gdf <- group_by(wcgs, cut(linpred, breaks = unique(quantile(linpred, (1:100)/101)))) )


( diagdf <- summarise(gdf, residuals = mean(residuals), linpred = mean(linpred)) )



# ----------
# deviance residuals are not constrained to have mean zero so the mean level in the plot is not of interest.
plot(residuals ~ linpred, diagdf, xlab = "linear predictor")


# -->
# We see an even variation as the linear predictor varies so this plot reveals no inadequacy in the model.



# ----------
# Plot the binned residuals against the predictors

gdf <- group_by(wcgs, height)
diagdf <- summarise(gdf, residuals = mean(residuals))

ggplot(diagdf, aes(x = height, y = residuals)) + geom_point()



# ----------
filter(wcgs, height == 77) %>% select(height, cigs, chd, residuals)
group_by(wcgs, cigs) %>% summarise(residuals = mean(residuals), count = n()) %>% ggplot(aes(x = cigs, y = residuals, size = sqrt(count))) + geom_point()



# ------------------------------------------------------------------------------
# deviance residuals is NOT normally distributed
# ------------------------------------------------------------------------------
qqnorm(residuals(lmod))
# qqnorm(residuals(lmod, "pearson"))



# ------------------------------------------------------------------------------
# Detect unusual cases:  leverages
# ------------------------------------------------------------------------------

halfnorm(hatvalues(lmod))


# The two outlying points can be identified
# These are the two men with the very highest cigarette consumption
filter(wcgs, hatvalues(lmod) > 0.015) %>% dplyr::select(height, cigs, chd)



# -->
# Given the relatively large size of the dataset and the fact that these two points are not particularly extreme, we are not concerned.




