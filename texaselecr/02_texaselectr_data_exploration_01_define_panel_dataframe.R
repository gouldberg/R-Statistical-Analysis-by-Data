setwd("//media//kswada//MyFiles//R//texaselectr")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
#  - This data were used by Kumbhakar (1996) and Horrace and Schmidt (1996) and concern the production cost of electric firms in Texas.
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)



# ----------
TexasElectr$cost <- with(TexasElectr, explab + expfuel + expcap)



# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


TE <- pdata.frame(TexasElectr)



# ----------
# This is unbalanced panel
pdim(TE)



# ----------
head(index(TE))



# ----------
table(index(TE)$id, useNA = "always")

table(index(TE)$year, useNA = "always")

table(index(TE))


