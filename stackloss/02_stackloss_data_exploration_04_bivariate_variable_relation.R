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
# Data Exploration:  Marginal plots by scatterplotMatrix
# ------------------------------------------------------------------------------


library(car)

formula <- ~ Air.Flow + Water.Temp + Acid.Conc. + stack.loss


scatterplotMatrix(formula, data = dat,
                  smooth = TRUE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)





# ----------

psych::pairs.panels(dat, stars = TRUE, method = "spearman")