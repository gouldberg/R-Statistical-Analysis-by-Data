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
# default diagnostics
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(mod1)




# ------------------------------------------------------------------------------
# residual plot
# ------------------------------------------------------------------------------


car::residualPlots(mod1)




# ------------------------------------------------------------------------------
# Adding poly(.,2)
# ------------------------------------------------------------------------------


mod2 <- lm(stack.loss ~ poly(Air.Flow,2) + poly(Water.Temp,2) + poly(Acid.Conc.,2), data = dat)


summary(mod2)




# ----------
mod3 <- lm(stack.loss ~ Air.Flow + poly(Water.Temp,2), data = dat)


summary(mod3)



car::Anova(mod3)


anova(mod3, mod1)



plot(mod3)


car::residualPlots(mod3)




# ------------------------------------------------------------------------------
# influence measures
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod3, vars = c("Cook", "studentized", "hat"), id.n = 4)




# ----------
# threshold value for hatvalues

k <- 3

n <- nrow(dat)

2:3 * k / n



# ----------
# 1,2,21 for hatvalues
# 3,4 for studentized residuals

obj <- c(1,2,3,4,21)

dat[obj,]




# ------------------------------------------------------------------------------
# Finding jointly influential observations by added-variable plots
# ------------------------------------------------------------------------------

graphics.off()


car::avPlots(mod3, id = TRUE, pch = 20, cex = 1.2, cex.lab = 1.5)




# ----------
mod_del <- lm(stack.loss ~ Air.Flow + poly(Water.Temp, 2), data = dat[c(-3,-4,-21),])


coef(mod_del)


coef(mod3)


