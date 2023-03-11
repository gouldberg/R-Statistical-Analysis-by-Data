setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)




# ------------------------------------------------------------------------------
# Time and Individual random effects model
# ------------------------------------------------------------------------------


wh <- plm(log(output) ~ log(labor) + log(machine), Tl.p, model = "random", random.method = "walhus", effect = "twoways")



# ----------
am <- update(wh, random.method = "amemiya")

sa <- update(wh, random.method = "swar")




# ----------
ercomp(sa)



# -->
# The shares of the individual and the time effects in the total error variance are now about 19 and 5% for the Swamy-Arora estimator




# ----------
re.models <- list(walhus = wh, amemiya = am, swar = sa)


sapply(re.models, function(x) sqrt(ercomp(x)$sigma2))




