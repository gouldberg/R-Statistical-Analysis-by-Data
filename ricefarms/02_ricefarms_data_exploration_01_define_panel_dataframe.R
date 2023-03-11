setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)


car::some(RiceFarms)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

library(plm)


Rice <- pdata.frame(RiceFarms, index = "id")



# ----------
# This is balanced panel
pdim(Rice)



# ----------
head(index(Rice))



# ----------
table(index(Rice)$id, useNA = "always")

table(index(Rice)$time, useNA = "always")

table(index(Rice))


