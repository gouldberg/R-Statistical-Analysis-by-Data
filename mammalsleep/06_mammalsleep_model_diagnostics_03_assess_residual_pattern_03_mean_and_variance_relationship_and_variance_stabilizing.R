setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
summary(qlmod)
summary(qlmod2b)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)

mod_obj <- qlmod
# mod_obj <- qlmod2b



# ------------------------------------------------------------------------------
# Testing for nonconstant error pattern
#   - Breusch and Pagan (1979) and R. D. Cook and Weisberg (1983) suggest a score test for nonconstant error variance
# ------------------------------------------------------------------------------

# But this applies only lm object ...
car::ncvTest(mod_obj, ~ log(body))



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
tmp_mod <- update(mod_obj, pdr^1.6 ~ .)

car::spreadLevelPlot(tmp_mod)



# ----------
residualPlots(tmp_mod, terms = ~1)

