setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# mod_obj <- modp.step2
mod_obj <- modp2
# mod_obj <- mod.nbin



# ------------------------------------------------------------------------------
# Testing for nonconstant error pattern
#   - Breusch and Pagan (1979) and R. D. Cook and Weisberg (1983) suggest a score test for nonconstant error variance
# ------------------------------------------------------------------------------

# But this applies only lm object ...
car::ncvTest(mod_obj, ~ log(Area))



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
tmp_mod <- update(mod_obj, log(Species)^1.016351 ~ .)

car::spreadLevelPlot(tmp_mod)



# ----------
residualPlots(tmp_mod, terms = ~1)

