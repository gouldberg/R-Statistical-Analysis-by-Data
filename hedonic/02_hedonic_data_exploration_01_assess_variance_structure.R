setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)



# ------------------------------------------------------------------------------
# Cross sectional correlation
# ------------------------------------------------------------------------------

library(plm);


hedp <- pdata.frame(Hedonic, index = "townid")


pdim(hedp)


head(index(hedp))



# ----------
# This is really unbalanced panel
# some town has only 1 time point, others have 30 time points 
table(index(hedp)$townid)



# ----------
# Variance structure of the series for dependent variable "mv"
summary(hedp$mv)

# summary(log(hedp$mv))



