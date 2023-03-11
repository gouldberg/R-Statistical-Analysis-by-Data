setwd("//media//kswada//MyFiles//R//privacy")

packages <- c("dplyr", "MPsychoR", "corrplot", "BayesFM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Privacy
# ------------------------------------------------------------------------------

data("Privacy", package = "MPsychoR")


str(Privacy)



# ----------
Privstd <- scale(Privacy)

head(Privstd)



# ------------------------------------------------------------------------------
# Simple principal component analysis
# ------------------------------------------------------------------------------

# Since the all items are on the same 1-100 scale, we could fit either a PCA without standardization or a PCA with standardization.

pcaPriv <- prcomp(Privacy, scale = TRUE)

summary(pcaPriv)


screeplot(pcaPriv, type = "lines", main = "Privacy Scree Plot")
abline(h = 1, col = "gray", lty = 2)


# -->
# The scree plot does not really show a clear elbow.
# The eigenvalues drop below 1 after 3 components.
# With 3 components we explain 59% of the variance.



# ----------
# explore loadings with p = 3
round(pcaPriv$rotation[, 1:3], 3)



# -->
# The second component is the easiest one to interpret since all the disadvantage items load highly on it.
# The advantage items load highly on component 1.
# The third component discriminates between the first three apc items and the remaining three apc items.



# ------------------------------------------------------------------------------
# Viualization of PCA results
# ------------------------------------------------------------------------------

biplot(pcaPriv, col = c("gray", "black"), cex = c(0.6, 0.9))
abline(h = 0, v = 0, lty = 2)

