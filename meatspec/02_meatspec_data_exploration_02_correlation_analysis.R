# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------

data(meatspec, package="faraway")


str(meatspec)


dim(meatspec)




# ------------------------------------------------------------------------------
# data exploration:  pairwise high correlated variables
# ------------------------------------------------------------------------------

# find variables with pairwise high correlation (absolute value)

corThresh <- 0.9


tooHigh <- caret::findCorrelation(cor(meatspec), corThresh)


names(meatspec)[tooHigh]



# -->
# Many variables are correlated .... difficult for normal regression


