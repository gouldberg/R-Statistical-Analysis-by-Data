setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ----------
mod_obj <- lmod



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals by index  (with Standardized Pearson Residuals)
# ------------------------------------------------------------------------------

# studentized Residuals
stud.resid <- rstudent(mod_obj)


# standardized Pearson residuals
stand.resid <- rstandard(mod_obj, type = "pearson")



# ----------
par(mfrow=c(2,1))
plot(stud.resid, ylim = c(min(-3, stud.resid), max(3, stud.resid)), main = "Studentized residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))




# ------------------------------------------------------------------------------
# Studentized Residuals
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod_obj, vars = c("studentized"), id.n = 4)



# ------------------------------------------------------------------------------
# Half-normal Plot by faraway::halfnorm
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

faraway::halfnorm(rstudent(mod_obj))


mammalsleep[c(11,2),]



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(mod_obj)




# ------------------------------------------------------------------------------
# Examine distribution of residuals
# ------------------------------------------------------------------------------

res <- rstudent(mod_obj)


# ----------
plot(density(res), lwd = 2, col = "blue")

rug(res)



# ----------
# Why the 3 modality ??  -->  damage or not
plot(jitter(orings$damage, factor = 1.5), res, xlab = "damage", ylab = "Studentized residual")


