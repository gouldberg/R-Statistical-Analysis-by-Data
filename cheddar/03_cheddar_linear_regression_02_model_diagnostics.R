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
# default diagnostics
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(mod1)




# ------------------------------------------------------------------------------
# residual plot
# ------------------------------------------------------------------------------


car::residualPlots(mod1)




# ------------------------------------------------------------------------------
# influence measures
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod1, vars = c("Cook", "studentized", "hat"), id.n = 4)




# ----------
# threshold value for hatvalues

k <- 3

n <- nrow(dat)

2:3 * k / n



# ----------
# 6,7 for hatvalues
# 15 for studentized residuals

obj <- c(6,7,15)

dat[obj,]




# ------------------------------------------------------------------------------
# Finding jointly influential observations by added-variable plots
# ------------------------------------------------------------------------------

graphics.off()


car::avPlots(mod1, id = TRUE, pch = 20, cex = 1.2, cex.lab = 1.5)




# ----------
mod_del <- lm(taste ~ H2S + Lactic, data = dat[c(-8,-15),])


coef(mod_del)

coef(mod1)


