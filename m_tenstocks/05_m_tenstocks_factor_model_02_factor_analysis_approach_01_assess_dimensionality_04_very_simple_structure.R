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
# Determining the number of factors:  very simple structure (VSS)
#
#   - VSS fits factor models by varying the number of factors systematically to achieve simple structure
#   - We define a "VSS complexity" denoting the amount of non-zero loadings of a variable in the model fit.
# ------------------------------------------------------------------------------
# c: ("VSS complexity") and VSS:
#   - denoting the amount of non-zero loadings of a variable in the model fit.
#   - c = 2 means that we only consider two non-zero loadings per variable
#   - By default the VSS function prints out the maximum VSS values for c = 1 and c = 2
#   - The higher VSS, the better the simple loadins structure represents out data.
#   - For c = 1, it suggests a one-factor solution, where as for c = 2 a two-factor solution is a good choice.
#
# MAP: minimum average partial
#   - the basic idea is to partial out the factors/components from the input correlation matrix
#   - The smaller the partial correlations (and therefore the sum-of-squares), the smaller the MAP and the better fit.
#
# RMSR: root mean squared residual
#   - the smaller the RMSR, the better the fit.
#
# RMSEA: root mean squared error of approximation
#   - based on the discrepancy function of the ML estimation.
#   - values smaller than 0.05 indicate good fit, 0.06-0.08 fair fit, and values larger than 0.10 poor fit
# ------------------------------------------------------------------------------

resvss <- VSS(polcor, fm = "ml", n.obs = nrow(rtn), plot = FALSE)


resvss


plot(resvss)



# --> 
# VSS:  2 factor solutions
# MAP:  1 factor
# RMSEA:  0.132 for 3 factor,  0.079 for 4 factor
# BIC:  4 factor

