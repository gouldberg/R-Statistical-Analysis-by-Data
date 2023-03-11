setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
icu.step2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)

mod_obj <- icu.step2



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals by index  (with Standardized Pearson Residuals)
# ------------------------------------------------------------------------------

# studentized Residuals
stud.resid <- rstudent(mod_obj)


# standardized Pearson residuals
stand.resid <- rstandard(mod_obj, type = "pearson")



# ----------
par(mfrow=c(2,1))
plot(stud.resid, ylim = c(min(-3, stud.resid), max(3, stud.resid)), main = "Studentized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "h")
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


ICU2[c(151,165),]



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
# Why the 3 modality ??
plot(ICU2$died, res, xlab = "ICU2", ylab = "Studentized residual")


