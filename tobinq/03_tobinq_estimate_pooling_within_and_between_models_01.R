setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")

str(TobinQ)

dim(TobinQ)



# ------------------------------------------------------------------------------
# Estimate three models:  pooling, within, and between
# ------------------------------------------------------------------------------

pTobinQ <- pdata.frame(TobinQ)


# ----------
pdim(pTobinQ)


index(pTobinQ)




# ----------
# Estimate pooling, within and between models
Qeq <- ikn ~ qn


Q.pooling <- plm(Qeq, pTobinQ, model = "pooling")


Q.within <- update(Q.pooling, model = "within")


Q.between <- update(Q.pooling, model = "between")




# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------
summary(Q.pooling)


summary(Q.within)


summary(Q.between)



# ----------
stargazer::stargazer(Q.pooling, Q.within, Q.between, intercept.bottom = FALSE, type = "text")



