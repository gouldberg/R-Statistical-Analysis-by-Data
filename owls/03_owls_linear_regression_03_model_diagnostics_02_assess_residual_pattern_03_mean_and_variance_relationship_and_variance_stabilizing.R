# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ----------
M.lm <- lm(NegPerChick ~ SexParent * FoodTreatment + SexParent * ArrivalTime, data = Owls)


mod_obj <- M.lm



# ------------------------------------------------------------------------------
# Testing for nonconstant error pattern
#   - Breusch and Pagan (1979) and R. D. Cook and Weisberg (1983) suggest a score test for nonconstant error variance
# ------------------------------------------------------------------------------

car::ncvTest(mod_obj)


car::ncvTest(mod_obj, ~ ArrivalTime)



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
tmp_mod <- update(mod_obj, NegPerChick^05580. ~ .)

car::spreadLevelPlot(tmp_mod)



# ----------
residualPlots(tmp_mod, terms = ~1)

