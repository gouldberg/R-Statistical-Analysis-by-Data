# ------------------------------------------------------------------------------
# Model Resident Satisfaction
#
# Polytomous Responses 
# Data is Frequency Form
# Variables are all category
# 
# Adding interaction term decreases AIC --- search interaction term by stepAIC
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "popbio", "effects", "MASS", "car")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- housing
str(data)
car::some(data)

describe(data)


# ------------------------------------------------------------------------------
# Ordered Factors
# ------------------------------------------------------------------------------
data$Sat <- ordered(data$Sat, levels = c("Low", "Medium", "High"))


# ------------------------------------------------------------------------------
# Basics: Satisfaction ~ Influence
# ------------------------------------------------------------------------------
( tmp <- xtabs(Freq ~ Infl + Sat, data = data) )
round(prop.table(tmp, margin = 1), 4)

spineplot(tmp)

mosaic(tmp, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

set.seed(1234)
mosaic(tmp, gp=shading_max, margin=c(right = 1))

interp <- function(x) pmin(x / 6, 1)
mosaic(tmp, shade = TRUE, gp_args = list(interpolate = interp))


assocstats(tmp)
CMHtest(tmp)

( OR <- loddsratio(tmp, log = FALSE) )
( LOR <- loddsratio(tmp, log = TRUE) )
summary(LOR)

confint(OR)
confint(LOR)

# fisher.test(tmp)


# ------------------------------------------------------------------------------
# Basics: Satisfaction ~ Cont
# ------------------------------------------------------------------------------
( tmp <- xtabs(Freq ~ Cont + Sat, data = data) )
round(prop.table(tmp, margin = 1), 4)

spineplot(tmp)

mosaic(tmp, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

set.seed(1234)
mosaic(tmp, gp=shading_max, margin=c(right = 1))

interp <- function(x) pmin(x / 6, 1)
mosaic(tmp, shade = TRUE, gp_args = list(interpolate = interp))


assocstats(tmp)
CMHtest(tmp)

( OR <- loddsratio(tmp, log = FALSE) )
( LOR <- loddsratio(tmp, log = TRUE) )
summary(LOR)

confint(OR)
confint(LOR)

# fisher.test(tmp)


# ------------------------------------------------------------------------------
# Basics: Satisfaction ~ Type
# ------------------------------------------------------------------------------
( tmp <- xtabs(Freq ~ Type + Sat, data = data) )
round(prop.table(tmp, margin = 1), 4)

spineplot(tmp)

mosaic(tmp, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

set.seed(1234)
mosaic(tmp, gp=shading_max, margin=c(right = 1))

interp <- function(x) pmin(x / 6, 1)
mosaic(tmp, shade = TRUE, gp_args = list(interpolate = interp))


assocstats(tmp)
CMHtest(tmp)

( OR <- loddsratio(tmp, log = FALSE) )
( LOR <- loddsratio(tmp, log = TRUE) )
summary(LOR)

confint(OR)
confint(LOR)

# fisher.test(tmp)


# ------------------------------------------------------------------------------
# Graphic Assessment of Proportional Odds Assumption --> !!! plot.xmean.ordinaly CAN NOT be used because can not specify weights = Freq .......
# ------------------------------------------------------------------------------
dd <- datadist(data);  options(datadist="dd")

plot.xmean.ordinaly(Sat ~ Infl + Cont + Type, data = data, lwd = 2, pch = 16, subn = TRUE, weights = Freq)


# ------------------------------------------------------------------------------
# Fitting the Proportional Odds Model (by polr())
# ------------------------------------------------------------------------------
# specify HEss = True to have the function return the observed information matrix (hessian), that is used in other operations to calculate standard errors
modpo_polr <- MASS::polr(Sat ~ Infl + Cont + Type, data = data, Hess = TRUE, method = "logistic", weights = Freq)
summary(modpo_polr)


# test for coefficients and intercept  -->  All variables are significant
car::Anova(modpo_polr)


# ------------------------------------------------------------------------------
# Compare
#  - Proportional Odds Model
#  - Non-Proportional Odds Model
# ------------------------------------------------------------------------------
modpo_vglm <- VGAM::vglm(Sat ~ Infl + Cont + Type, data = data, family = cumulative(link = "logit", parallel = TRUE), weights = Freq)
modnpo_vglm <- VGAM::vglm(Sat ~ Infl + Cont + Type, data = data, family = cumulative(link = "logit"), weights = Freq)

summary(modpo_vglm)
summary(modnpo_vglm)


coef(modpo_vglm, matrix = TRUE)
coef(modnpo_vglm, matrix = TRUE)


AIC(modpo_vglm) # --> better for proportional odds models
AIC(modnpo_vglm)

VGAM::lrtest(modpo_vglm, modnpo_vglm)  # --> but the difference is not significant


# ------------------------------------------------------------------------------
# Search for Interaction Effects
# ------------------------------------------------------------------------------
modpo_polr_inter <- MASS::polr(Sat ~ (Infl + Cont + Type)^2, data = data, Hess = TRUE, method = "logistic", weights = Freq)
summary(modpo_polr_inter)

search_model <- stepAIC(modpo_polr_inter, direction="both")

search_model$call


modpo_polr_inter <- MASS::polr(Sat ~ Infl + Cont + Type + Infl:Type + Cont:Type, data = data, Hess = TRUE, method ="logistic", weights = Freq)
summary(modpo_polr_inter)
car::Anova(modpo_polr_inter)  # --> all significant


AIC(modpo_polr_inter) # --> best
AIC(modpo_polr)


# ------------------------------------------------------------------------------
# Model by lrm()
# ------------------------------------------------------------------------------
dd <- datadist(data);  options(datadist="dd");

modpo_lrm <- lrm(Sat ~ Infl + Cont + Type + Infl:Type + Cont:Type, data = data, weights = Freq, x = TRUE, y = TRUE)
print(modpo_lrm, coefs=5)
AIC(modpo_lrm)


# Wald-ANOVA table
print(anova(modpo_lrm))


# relative contribution
plot(anova(modpo_lrm))


( s <- modpo_lrm$stats )
( gamma.hat <- (s["Model L.R."] - s["d.f."]) / s["Model L.R."] )
( 1 - gamma.hat )


# Odds Ratios
plot(summary(modpo_lrm), log=TRUE)



# ------------------------------------------------------------------------------
# Visualizing results for the proportional odds model
# ------------------------------------------------------------------------------
tmp <- cbind(data, predict(modpo_polr, type="probs"))

tmp <- reshape2::melt(tmp,
            id.vars = c("Sat", "Infl", "Cont", "Type"),
            measure.vars = c("Low", "Medium", "High"),
            variable.name = "Level",
            value.name = "Probability")

head(tmp)


gg <- ggplot(tmp, aes(x = Infl, y = Probability, colour = Level)) + 
  geom_line(size = 2.5) + theme_bw() +
  geom_point(color = "black", size = 1.5) + facet_grid(Cont ~ Type)
gg


# ------------------------------------------------------------------------------
# Effect Plots
# ------------------------------------------------------------------------------
library(effects)

plot(Effect("Infl", modpo_polr))

plot(Effect("Infl", modpo_polr), style = "stacked", key.args = list(x = 0.55, y = 0.9))


plot(Effect(c("Infl", "Cont"), modpo_polr), style = "stacked", key.args = list(x = 0.8, y = 0.9))
plot(Effect(c("Infl", "Type"), modpo_polr), style = "stacked", key.args = list(x = 0.8, y = 0.9))
plot(Effect(c("Cont", "Type"), modpo_polr), style = "stacked", key.args = list(x = 0.8, y = 0.9))

plot(Effect(c("Infl", "Cont"), modpo_polr, latent = TRUE), lwd = 3)
plot(Effect(c("Infl", "Type"), modpo_polr, latent = TRUE), lwd = 3)
plot(Effect(c("Cont", "Type"), modpo_polr, latent = TRUE), lwd = 3)
