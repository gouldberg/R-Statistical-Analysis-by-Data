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
levels(data$cancer) <- c("-", "Cancer")
levels(data$admit) <- c("-", "Emerg")
levels(data$uncons) <- c("-", "Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

icu.fitp <- cbind(ICU2, Probability = predict(icu.glm2, type = "response"))


head(icu.fitp)




library(ggplot2)


# library(directlabels)

graphics.off()

# gg <- ggplot(plotdat, aes(x = age, y = Probability)) + theme_bw() + geom_line(size = 2.5, aes(color = sex)) + geom_point(colour = "black", size = 1.5) +
#  facet_grid(~ sex + admit, labeller = function(x, y) sprintf("%s = %s", x, y))

gg <- ggplot(icu.fitp, aes(x = age, y = Probability)) + theme_bw() + geom_line(size = 2.5, aes(color = sex)) + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ sex + admit)


# direct.label(gg)

