setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))

fam <- data$family
levels(fam)[c(3,4,6,7,9)] <- "Others"

fam <- factor(fam, levels(fam)[c(1, 2, 4:6, 3)])
data$family <- fam



# ----------
# For illustrative purposes,
# we consider the influence measures and diagnostic plots for one specific model, the model donner.mod3,
# which included a quadratic effect of age and a main effect of sex, but no interaction

donner.mod3 <- glm(survived ~ poly(age, 2) + sex, data = data, family = binomial)



# ------------------------------------------------------------------------------
# Overview of adequacy of a fitted model
# ------------------------------------------------------------------------------
# We consider the residual-leverage graph (number 5) as being the most useful for asessing influential observations, 
# plotting residuals against leverages.

plot(donner.mod3)

plot(donner.mod3, which = 5)



# ------------------------------------------------------------------------------
# influential statistics for each case
# ------------------------------------------------------------------------------
infl <- influence.measures(donner.mod3)


# show influence measures for each case
# The summary() method for the "infl" object prints those observartions considered noteworthy on one or more of these statistics,
# as indicated by a "*" next to the value.
summary(infl)



# ------------------------------------------------------------------------------
# Extended version of plotting residuals against leverages
# ------------------------------------------------------------------------------
# An extended version is produced by the function car::influencePlot(),
# which additionally uses the size (area) of the plotting symbol to also show the value of Cook's D.

op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(donner.mod3, id.col = "blue", scale = 8, id.n = 2)
k <- length(coef(donner.mod3))
n <- nrow(data)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(data) %in% rownames(res))

cbind(data[idx, 2:4], res)



# -->
# We can see that Patrick Breen and James Reed are unusual because they were both older men who survived, and have large positive residuals.
# Breen is the most influential by Cook's D, but this value is not excessively large.
# The two women were among the older women who died. They are selected here because they have the largest hat values, measning they are unusual in
# terms of the distribution of age and sex, but they are not particularly influential in terms of Cook's D.



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------
car::influenceIndexPlot(donner.mod3, vars = c("Cook", "studentized", "hat"), id.n = 4)


