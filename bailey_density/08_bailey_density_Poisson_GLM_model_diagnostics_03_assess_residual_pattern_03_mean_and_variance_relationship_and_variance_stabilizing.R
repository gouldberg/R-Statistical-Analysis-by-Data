# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ----------
mod_obj <- M3



# ------------------------------------------------------------------------------
# Testing for nonconstant error pattern
#   - Breusch and Pagan (1979) and R. D. Cook and Weisberg (1983) suggest a score test for nonconstant error variance
# ------------------------------------------------------------------------------

# But this applies only lm object ...
car::ncvTest(mod_obj, ~ MeandDepth)



# ------------------------------------------------------------------------------
# Assess variance-stabilizing power transformation
#   - car::spreadLevelPlot() generalizes Tukey's spread-level plot for exploratory data analysis
#   - Plotting log absolute studentized residuals versus log fitted values.
#     A positive relationship in the spread-level plot indicates a tendency of residual variation to increase with the magnitude of the response,
#     and the slope of a line fit to the plot (say b) can be used to select a variance-stabilizing power transformation (the 1 - b power).
#   - The spread-level plot requires a positive response and positive fitted values;  negative fitted values are ignored
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


car::spreadLevelPlot(mod_obj)



# ----------
# updating by suggested power transformation
tmp_mod <- update(mod_obj, TotAbund^0.5464108 ~ .)

car::spreadLevelPlot(tmp_mod)



# ----------
residualPlots(tmp_mod, terms = ~1)

