setwd("//media//kswada//MyFiles//R//asti")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ASTI
# ------------------------------------------------------------------------------

data("ASTI", package = "MPsychoR")


str(ASTI)



# ----------
st <- ASTI[, c(2, 4, 7, 13, 16, 24, 25)]
pg <- ASTI[, c(11, 14, 15, 17, 18, 23)]


stpg <- data.frame(st = st, pg = pg)
  


# ------------------------------------------------------------------------------
# Standard principal component analysis
# ------------------------------------------------------------------------------

# This implies that we treat the items on a metric scale level
# meaning that we perform a linear transformation.
# This function merely standardized the variables: it does not change any of the interval scale properties

pcafit <- prcomp(stpg, scale = TRUE)

summary(pcafit)


screeplot(pcafit, type = "lines")


pcafit$rotation
