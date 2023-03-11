setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# Linear regression
#    - We model the Shannon index as a function of the covariate Percentage of Herb Layer Cover
# ------------------------------------------------------------------------------

M0 <- lm(Hlog10 ~ HerbLayer, data = Spiders)


summary(M0)



# ----------
plot(x = Spiders$HerbLayer, y = Spiders$Hlog10, xlab = "Percentage of herb layer", ylab = "Shannon index", cex.lab = 1.5, pch = 16)

abline(M0, lwd = 3)



# ------------------------------------------------------------------------------
# assess "Plot" effect from residuals
# ------------------------------------------------------------------------------

E0 <- resid(M0)

par(mar = c(5,5,2,2))

boxplot(E0 ~ fPlot, data = Spiders, xlab = "Plot", ylab = "Residuals", cex.lab = 1.5)

abline(h = 0, lty = 2)


# -->
# Clear Plot effect in the residuals



# ------------------------------------------------------------------------------
# Include "Plot" as covariate
# ------------------------------------------------------------------------------

M1 <- lm(Hlog10 ~ HerbLayer + fPlot, data = Spiders)

summary(M1)


drop1(M1, test = "F")

AIC(M0, M1)


# -->
# The model explains 61% (vs. 4%) of the variation, indicating that the categorical covariate Plot provides considerable more information than
# does Percentage of Herb Layer Cover.
# F-test indicates that Plot is significant at the 5% level.

# REsidual standard error is 0.1234 on 121 degrees of freedom


# -->
plot(M1)


