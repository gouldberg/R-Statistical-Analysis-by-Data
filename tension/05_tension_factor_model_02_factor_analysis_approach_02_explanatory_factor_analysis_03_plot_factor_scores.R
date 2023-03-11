setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)


dim(tension)



# -----------
tens <- t(as.matrix(tension[, 1:800]))



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(tens))))


tens <- as.matrix(tens) %*% std




# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(tens)


polcor



# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


resFA3_scores <- factor.scores(tens, f = resFA3, method = "Thurstone")$scores


MTSplot(resFA3_scores)



# ----------
# for comparison

MTSplot(m1$scores[,1:3])




# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


par(mfrow = c(1,1), mar = c(2,2,2,2))


lattice::xyplot(ML2 ~ ML1, data = data.frame(resFA3_scores), pch = 20, col = "blue")


