setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Variance Explained (goodness of fit)
# ------------------------------------------------------------------------------

# proportion of deviance explained by this model
1 - modp$deviance / modp$null.deviance

1 - mod.qpois$deviance / mod.qpois$null.deviance

1 - mod.nbin$deviance / mod.nbin$null.deviance


# -->
# not improved by negative binomial model

