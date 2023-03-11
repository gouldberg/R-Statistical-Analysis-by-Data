setwd("//media//kswada//MyFiles//R//birthwt")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birthwt
#   - Data on 189 babies born at Baystate Medical Center, Springfield, MA during 1986.
#   - The quantitative response is bwt (birth weight in grams), and this is also recoded as low, a binary variable corresponding to bwt < 2500 (2.5 kg).
# ------------------------------------------------------------------------------
data("birthwt", package = "MASS")

data <- birthwt

dim(data)
str(data)


# ----------
Hmisc::describe(data)



# ----------
data$race <- factor(data$race, labels = c("white", "black", "other"))
data$ptd <- factor(data$ptl > 0)  # premature labors
data$ftv <- factor(data$ftv)  # physician visits
levels(data$ftv)[-(1:2)] <- "2+"
data$smoke <- factor(data$smoke > 0)
data$ht <- factor(data$ht > 0)
data$ui <- factor(data$ui > 0)

data$low <- factor(data$low, levels = c(0, 1))
data$bwt <- NULL
data$ptl <- NULL


levels(data$smoke) <- c("-", "smoke")
levels(data$ht) <- c("-", "ht")
# levels(data$ui) <- c("-", "ui")
levels(data$ptd) <- c("-", "ptd")


btw.final <- glm(low ~ age + race + smoke + ht + ptd, data = data, family = binomial)
btw.final2 <- glm(low ~ lwt + ht + ptd, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Overview of adequacy of a fitted model
# ------------------------------------------------------------------------------
# We consider the residual-leverage graph (number 5) as being the most useful for asessing influential observations, 
# plotting residuals against leverages.

plot(btw.final)

plot(btw.final, which = 5)
plot(btw.final2, which = 5)



# ------------------------------------------------------------------------------
# influential statistics for each case
# ------------------------------------------------------------------------------
infl <- influence.measures(btw.final)
infl2 <- influence.measures(btw.final2)


# show influence measures for each case
# The summary() method for the "infl" object prints those observartions considered noteworthy on one or more of these statistics,
# as indicated by a "*" next to the value.
summary(infl)
summary(infl2)



# ------------------------------------------------------------------------------
# Extended version of plotting residuals against leverages
# ------------------------------------------------------------------------------
# An extended version is produced by the function car::influencePlot(),
# which additionally uses the size (area) of the plotting symbol to also show the value of Cook's D.

op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(btw.final, id.col = "blue", scale = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(btw.final))
n <- nrow(data)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(data) %in% rownames(res))

cbind(data[idx, c("low", "age", "race", "smoke", "ht", "ptd")], res)



# -->
# None of the cases are particulary influential on the model coefficients overall: the largest Cook's D is only 0.037 for case 119



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------
car::influenceIndexPlot(btw.final, vars = c("Cook", "studentized", "hat"), id.n = 4)



