setwd("//media//kswada//MyFiles//R//neuralactivity")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  NeuralActivity
# ------------------------------------------------------------------------------

data("NeuralActivity", package = "MPsychoR")

str(NeuralActivity)


NeuralActivity



# ------------------------------------------------------------------------------
# Fit a single MDS on averaged data across individuals
# ------------------------------------------------------------------------------

# Compute average dissimilarities across individuals
delta <- Reduce("+", NeuralActivity) / length(NeuralActivity)



# ----------
# fit a single MDS
library(smacof)

fit_neural <- smacof::mds(delta, type = "interval")

fit_neural



# ------------------------------------------------------------------------------
#  Multidimensional Scaling Biplots
#
#  - data:  NeuralScales
#     - The external scales based on questionnaire data and contain proportions, telling us to which degree people associate
#       each of the 60 mental states with 16 theoretical dimensions the authors extracted from the literature.
# ------------------------------------------------------------------------------

data("NeuralScales", package = "MPsychoR")



# ----------
# map these external variables on the MDS configuration.
# by defalt, the biplotmds function standardizes these variables and, therefore, returns the standardized regression coefficients.
# The direction and length of the vectors is determined by the underlying regression parameters, just as in the regression biplot.

mdsbi <- biplotmds(fit_neural, NeuralScales)


mdsbi$coef

mdsbi$R2vec


plot(mdsbi, main = "Neural Activity MDS Biplot", col = "gray", label.conf = list(col = "gray"), 
     vec.conf = list(col = "brown", cex = 1))




# ------------------------------------------------------------------------------
# Correspondng biplot axis with projections
# ------------------------------------------------------------------------------
# focus on the theoretical dimension "emotion", since this is the regression with the highest R^2 value (0.438)

X <- fit_neural$conf

Y <- scale(NeuralScales, scale = TRUE)

plot(X, pch = 20, cex = 0.6, xlab = "Dimension 1", ylab = "Dimension 2", col = "darkblue", asp = 1, main = "Biplot MDS Emotion Axis")
text(X, labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "gray")
calEm <- calibrate(mdsbi$coef[,"Emotion"], Y[,"Emotion"], tm = seq(-2, 1.5, by = 0.5), Fr = X, 
                   dp = TRUE, axiscol = "brown", axislab = "Emotion", labpos = 3, verb = FALSE)


# -->
# States like laziness, agitation, disgust, and awe
# are the most important mental states determining the emotion dimension.
# Cognition is the least important state.



