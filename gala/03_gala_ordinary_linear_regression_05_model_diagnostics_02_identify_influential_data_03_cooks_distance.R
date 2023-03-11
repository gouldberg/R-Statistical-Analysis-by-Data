]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


# Baltra deleted
lmod_del <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[-1,])




# ------------------------------------------------------------------------------
# Identify influential observations:  Cook's distance
# ------------------------------------------------------------------------------


cooks.distance(lmod)




# ----------
# for Baltra

cooks.distance(lmod)[1]


# weghted sum of squares of the differences between coefficients
( coef_diff <- coef(lmod) - coef(lmod_del) )

coef_diff %*% ( t(x) %*% x ) %*% coef_diff / (length(coef(lmod)) * sigma2)



# outlyingness * leverage
rstandard(lmod)[1]^2 / length(coef(lmod)) * hat[1] / (1 - hat[1]) 




# ----------

crit <- 1

cooks.distance(lmod)[cooks.distance(lmod) > crit]



