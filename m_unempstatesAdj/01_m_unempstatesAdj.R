# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
#   - monthly unemployment rates of the U.S. 50 states from January 1976 to August 2010 for 416 observations
#   - Preliminary analysis indicates the existence of some large outliers and we made some simple adjustments
#        - Arizona:  Due to a level shift, we subtract 3.313 from each of the first 92 observations
#        - Louisiana:  To adjust for the effect of Hurricane Katrina, we subtracted (6,6,6,1) from the unemployment rates for
#          t from 357 to 360, respectively
#        - Mississippi: To partially remove the effect of Hurricane Katrina, we subtracted (3, 2.5, 3, 2, 0.5) from the
#          observed rates for t from 357 to 361, respectively
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


da[357:361,]

