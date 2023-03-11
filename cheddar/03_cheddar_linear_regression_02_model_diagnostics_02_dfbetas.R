# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# dfbetas
# ------------------------------------------------------------------------------

crit <- 0.25

abs(dfbeta(mod1)) >= crit

( abs(dfbeta(mod1)) >= crit ) * 100



# -->
# note that almost all data for dfbetas for Lactic is over 0.25 or uner -0.25 ...




( tmp <- data.frame(dfbeta(mod1)) )


formula <- ~ H2S + Lactic

car::scatterplotMatrix(formula, data = tmp,
                       smooth = FALSE,
                       id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)

