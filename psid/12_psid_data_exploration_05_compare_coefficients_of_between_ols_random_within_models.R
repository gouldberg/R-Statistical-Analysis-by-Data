setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")

str(psid)

car::some(psid)



# ----------
library(plm)

ps.p <- pdata.frame(psid, index = c("person", "year"))



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(log(income) ~ as.numeric(year) + as.numeric(age) + sex + as.numeric(educ), data = ps.p, model = x))["as.numeric(year)"])

