setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")

str(psid)

car::some(psid)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)

ps.p <- pdata.frame(psid, index = c("person", "year"))



# ----------
# This is unbalanced panel
pdim(ps.p)



# ----------
head(index(ps.p))



# ----------
table(index(ps.p)$person, useNA = "always")

table(index(ps.p)$year, useNA = "always")

table(index(ps.p))


