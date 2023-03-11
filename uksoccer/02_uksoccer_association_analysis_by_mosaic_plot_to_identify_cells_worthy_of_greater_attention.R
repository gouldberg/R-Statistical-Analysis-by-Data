setwd("//media//kswada//MyFiles//R//uksoccer")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKSoccer
# ------------------------------------------------------------------------------
data("UKSoccer", package = "vcd")

data <- UKSoccer

data
sum(data)

str(data)



# ------------------------------------------------------------------------------
# CMHtest
# ------------------------------------------------------------------------------
# Here, the rows and columns of the table are both ordered, so it is convenient and 
# compact to carry out all the CMH tests taking ordinality into account

CMHtest(data)


# --> all of these are non-significant



# ------------------------------------------------------------------------------
# masaic plot to identiry the cells worthy of greater attention for further analysis
#  - maximum residual test by shading_max  (coindep_test() is used for permutation test for conditional independence)
# ------------------------------------------------------------------------------
set.seed(1234)

mosaic(data, gp=shading_max, labeling = labeling_residuals, digits = 2)



# --> One residual, r(42) = 3.08 stands out,
# corresponding to 4 or more goals by the home team and only 2 goals by the away team,
# which accounts for nearly half of the X^2(16) = 18.7 for general association



