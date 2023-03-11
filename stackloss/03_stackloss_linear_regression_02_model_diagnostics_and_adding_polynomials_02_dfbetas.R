# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# dfbetas
# ------------------------------------------------------------------------------

crit <- 0.25

abs(dfbeta(mod3)) >= crit

( abs(dfbeta(mod3)) >= crit ) * 100



# -->
# note that almost all data for Water.Temp is over 0.25 or uner -0.25 ...




( tmp <- data.frame(dfbeta(mod3)) )


formula <- ~ Air.Flow + poly.Water.Temp..2.1 + poly.Water.Temp..2.2

car::scatterplotMatrix(formula, data = tmp,
                       smooth = FALSE,
                       id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)

