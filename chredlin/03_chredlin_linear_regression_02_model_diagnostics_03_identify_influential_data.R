setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)



# ------------------------------------------------------------------------------
# model diagnostics:  Studentized (deletion) residuals by index
# ------------------------------------------------------------------------------

# standardized Residuals
stand.resid <- rstandard(linmod)


# studentized Residuals
stud.resid <- rstudent(linmod)



# ----------
par(mfrow=c(2,1))
plot(stand.resid, ylim = c(min(-3, stand.resid4), max(3, stand.resid4)), main = "Standardized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stud.resid, ylim = c(min(-3, stud.resid4), max(3, stud.resid4)), main = "Studentized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
car::influenceIndexPlot(linmod, vars = c("studentized"))



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(linmod)




# ------------------------------------------------------------------------------
# model diagnostics:  Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
influencePlot(linmod)



# ----------
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(linmod, scale = 8)
k <- length(coef(linmod))
n <- nrow(chredlin)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(chredlin) %in% rownames(res))

cbind(chredlin[idx,], res)




# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(linmod, vars = c("Cook", "studentized", "hat"), id.n = 4)



