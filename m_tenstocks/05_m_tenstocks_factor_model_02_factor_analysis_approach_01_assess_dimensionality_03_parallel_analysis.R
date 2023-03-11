# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


polcor




# ------------------------------------------------------------------------------
# Determining the number of factors:  parallel analysis
#   - Parallel analysis performs a full model fit on the original dataset, on resampled data (bootstrap)
#     as well as on random, uncorrelated data matrices drawn from a normal distribution.
#   - In parallel analysis, the criterion to be used in order to determine the number of factors is the following:
#        - A factor is considered as "significant" if its eigenvalue is larger than the 95% quantile (red line) of those obtained from random or resample data.
#   - Parallel analysis is partially sensitive to sample size in that for large samples, the eigenvalues of random factors will tend to be
#     very small and thus the number of components or factors will tend to be more than using other rules.
# ------------------------------------------------------------------------------

set.seed(123)


# cor = "poly":  for polytonomous variables
resPA <- fa.parallel(rtn, fa = "pc", cor = "cor", fm = "ml")

resPA


plot(resPA, show.legend = TRUE, fa = "pc")



# -->
# The solid line is simply the scree plot based on observed data
# The red lines reflects the 95% quantiles of the bootstrapped data (resampled; dotted)
# and random data (generated from a normal distribution; dashed)

# In this example, we should pick 2 factors.

