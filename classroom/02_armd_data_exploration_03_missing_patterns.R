
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)




# ------------------------------------------------------------------------------
# Data exploration:  missing patterns
# ------------------------------------------------------------------------------

attach(armd.wide)

( miss.pat <- nlmeU:::missPat(visual4, visual12, visual24, visual52) )

detach(armd.wide)


table(miss.pat)



# -->
# There were 188 patients for whom all four post-randomization visual acuity measurements were obtained.
# It is also worth noting that there are eight (= 4 + 1 + 2 + 1) patients with four different nonmonotone missing-data patterns,
# i.e., with intermittent missing visual acuity measurements.
# When modeling data with such patterns, extra care is needed when specifying variance-covariance structures.



