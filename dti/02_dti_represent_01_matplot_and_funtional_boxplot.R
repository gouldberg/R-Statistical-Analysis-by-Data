setwd("//media//kswada//MyFiles//R//dti")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diffusion tensor imaging (DTI)
# ------------------------------------------------------------------------------
data("DTI", package = "refund")

str(DTI)



# ----------
( Y <- DTI$cca )


# exclude rows with missing values
Y <- Y[-c(126, 130, 131, 125, 319, 321),]



# ------------------------------------------------------------------------------
# Representation by matplot()
# ------------------------------------------------------------------------------

dim(Y)


argvals <- seq(0, 1, length = ncol(Y))

samp <- sample(nrow(Y), size = 10, replace=FALSE)

matplot(y = t(Y[samp,]), x = argvals, type = "l", 
        ylab = "Fractional Anisotoropy", xlab = "Location within corpus collusum (normalized to unit interval",
        main = "sampled")



# ------------------------------------------------------------------------------
# Represent by functional boxplot
#   - Functional boxplots are not obtained from pointwise boxplots for each day
#     a measure of centrality for each function compared to the other functions is used
# ------------------------------------------------------------------------------

fbplot(fit = t(Y),
       ylab = "Fractional Anisotoropy", xlab = "Location within corpus collusum (normalized to unit interval")

# -->
# Black line is the median curve.
# The magenta region represents the middle 50% of curves, and the dashed red curves are the outlying functions
# Note that functional boxplots produce outlying curves rather than individual outlying.



# ----------
# Three regions corresponding to probability levels 30, 60, and 90
par(mfrow=c(1,3))
fbplot(fit = t(Y), prob = 0.3)
fbplot(fit = t(Y), prob = 0.6)
fbplot(fit = t(Y), prob = 0.9)

