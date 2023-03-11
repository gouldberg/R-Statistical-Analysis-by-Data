setwd("//media//kswada//MyFiles//R//handwrit")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  handwrit
#   - X-Y coordinates of 20 replications of writing the script "fda", each replication is represented by 1401 coordinate values.
#     The scripts have been extensively pre-processed. They have been adjusted to a common length that corresponds to 2.3 seconds or 2300 millisecond,
#     and they have already been registered so that important features in each script are aligned.
#   - This analysis is designed to illustrate techniques for working with functional data having rather high frequency variation and represented by
#     thousands of data points per record.
#   - An interesting suprise in the results is the role placed by a 120 millisecond cycle such that sharp features such as cusps
#     correspond closely to this period.  This 110-120 msec cycle seems is usually seen in human movement data involving rapid
#     movements, such as speech, juggling and so on.
# ------------------------------------------------------------------------------
data("handwrit", package = "fda")

str(handwrit)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# X coodinates of 20 samples of handwriting 
handwrit[,,1]


# Y coodinates of 20 samples of handwriting 
handwrit[,,2]



# ----------
# The first 'fda' handwriting sample
i <- 1
matplot(100 * handwrit[, i, 1], 100 * handwrit[, i, 2], type="l", lty='solid', las=1, xlab='', ylab='')



# ----------
# 20 samples of handwriting are superimposed
matplot(100 * handwrit[, , 1], 100 * handwrit[, , 2], type="l", lty='solid', las=1, xlab='', ylab='')



