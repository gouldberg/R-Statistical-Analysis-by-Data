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




# ------------------------------------------------------------------------------
# models
# ------------------------------------------------------------------------------

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)

lmod_r <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala, subset = (row.names(gala) != "Isabela"))




# ------------------------------------------------------------------------------
# Exhaustively searches all possible combinations of the predictors by regsubsets
# ------------------------------------------------------------------------------

library(leaps)



# for each size of model p, it finds the variables that produce the minimum RSS
b <- regsubsets(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala, method = "exhaustive")

b



# ----------
( rs <- summary(b) )


rs$which



# ----------
( AIC <- nrow(gala) * log(rs$rss / nrow(gala)) + (1:5) * 2 )


plot(AIC ~ I(1:5), ylab = "AIC", xlab = "Number of Predictors")




# -->
# best model is only 2 predictors
# "Elevation" and "Adjacent"



# ----------
# final model
lmod_final <- lm(Species ~ Elevation + Adjacent, data = gala)

summary(lmod_final)



# ----------
# diagnostics

par(mfrow = c(2,2))

plot(lmod_final)



car::residualPlots(lmod_final)




# ------------------------------------------------------------------------------
# "Isabela" is removed
# ------------------------------------------------------------------------------

b_r <- regsubsets(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[-16,], method = "exhaustive")

b_r



# ----------
( rs_r <- summary(b_r) )


rs_r$which



# ----------
n <- nrow(gala) - 1

( AIC <- n * log(rs_r$rss / n) + (1:5) * 2 )


plot(AIC ~ I(1:5), ylab = "AIC", xlab = "Number of Predictors")



# ----------
lmod_r_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala[-16,])

summary(lmod_r_final)
