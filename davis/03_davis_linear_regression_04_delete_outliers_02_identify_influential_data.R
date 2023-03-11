rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)
linmod2 <- update(linmod, subset = -12)




# ------------------------------------------------------------------------------
# Studentized (deletion) residuals by index
# ------------------------------------------------------------------------------

# standardized Residuals
stand.resid2 <- rstandard(linmod2)

# studentized Residuals
stud.resid2 <- rstudent(linmod2)



# ----------
par(mfrow=c(2,1))
plot(stand.resid2, ylim = c(min(-3, stand.resid2), max(3, stand.resid2)), main = "Standardized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stud.resid2, ylim = c(min(-3, stud.resid2), max(3, stud.resid2)), main = "Studentized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
car::influenceIndexPlot(linmod2, vars = c("studentized"))



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(linmod2)




# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

influencePlot(linmod2)


# ----------
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(linmod2, scale = 8)
k <- length(coef(linmod2))
n <- nrow(data2[,-12])
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(data2) %in% rownames(res))

cbind(data2[idx,], res)




# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(linmod2, vars = c("Cook", "studentized", "hat"), id.n = 4)



