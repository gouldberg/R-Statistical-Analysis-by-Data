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



# ------------------------------------------------------------------------------
# Variance Explained (goodness of fit)
# ------------------------------------------------------------------------------

# proportion of deviance explained by this model
1 - modp2$deviance / modp2$null.deviance

1 - mod.qpois$deviance / mod.qpois$null.deviance

1 - mod.nbin$deviance / mod.nbin$null.deviance


# -->
# Negative Binomial Model is NOT improved, but deviance is increased ...

