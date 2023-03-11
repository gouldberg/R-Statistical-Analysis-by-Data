# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ----------
M1 <- lm(Testisweight ~ DML * fMONTH, data = Squid)

mod_obj <- M1



# ------------------------------------------------------------------------------
# Testing for nonconstant error pattern
#   - Breusch and Pagan (1979) and R. D. Cook and Weisberg (1983) suggest a score test for nonconstant error variance
# ------------------------------------------------------------------------------

car::ncvTest(mod_obj, ~ DML * fMONTH)



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
tmp_mod <- update(mod_obj, Testisweight^0.5825954 ~ .)

car::spreadLevelPlot(tmp_mod)



# ----------
residualPlots(tmp_mod, terms = ~1)

