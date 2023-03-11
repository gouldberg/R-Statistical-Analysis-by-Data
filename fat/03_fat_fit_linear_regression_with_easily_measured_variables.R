setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ------------------------------------------------------------------------------
# Fit linear regression only with age, weight, height and abdom
# which are either known by the individual or easily measured
# ------------------------------------------------------------------------------

lmod2 <- lm(brozek ~ age + weight + height + abdom, data = fat)


summary(lmod)
summary(lmod2)


# -->
# R2 = 0.7211, which is decreased from that of full model 0.749, but parsimonious.



# ------------------------------------------------------------------------------
# Check the median characteristics
# ------------------------------------------------------------------------------
# let's consider the typical man, exemplified by the median value of all the predictors

x2 <- model.matrix(lmod2)

head(x2)

( x02 <- apply(x2, 2, median) )

( y02 <- sum(x02 * coef(lmod2)))



# ----------
# estimated body fat of median characteristics are not much different (slightly increased from full model)
y0
y02



# ----------
# 95% prediction interval
# a little bit wider, but not much different
predict(lmod, newd = data.frame(t(x0)), interval = "prediction")
predict(lmod2, newd = data.frame(t(x02)), interval = "prediction")



# ----------
# 95% confidence interval:  not much different
predict(lmod, new = data.frame(t(x0)), interval = "confidence")
predict(lmod2, new = data.frame(t(x02)), interval = "confidence")




# ------------------------------------------------------------------------------
# Let's see the body fat value at characteristics at further from the original data
# ------------------------------------------------------------------------------

( x1_2 <- apply(x2, 2, function(x) quantile(x, 0.95)) )


predict(lmod, newd = data.frame(t(x1)), interval = "prediction")
predict(lmod2, newd = data.frame(t(x1_2)), interval = "prediction")


# -->
# also the width of 95% interval is not much different



# ------------------------------------------------------------------------------
# Check anomolous obeservations
# ------------------------------------------------------------------------------

# from case 25 to 50, case 39 and 42 seems to be anomalous
plot(lmod2)

fat[c(39,42),c("age", "weight", "height", "abdom")]



# ------------------------------------------------------------------------------
# Fit linear regression excluding those 2 anomalous observations and recompute 95% prediction interval
# ------------------------------------------------------------------------------

nrow(fat[-c(39,42),])

lmod2_e <- lm(brozek ~ age + weight + height + abdom, data = fat[-c(39,42),])

summary(lmod2_e)


x2_e <- model.matrix(lmod2_e)



# ----------
# 95% prediction interval
# prediction interval is a little bit narrower, but not much different.
predict(lmod2, newd = data.frame(t(x02)), interval = "prediction")
predict(lmod2_e, newd = data.frame(t(x02_e)), interval = "prediction")





