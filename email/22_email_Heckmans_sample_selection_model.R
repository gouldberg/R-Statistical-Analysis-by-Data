setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Heckman's Sample Selection Model
# ------------------------------------------------------------------------------


library(sampleSelection)


spend.heckit <- selection(selection = treatment ~ recency + history + channel + segment + visit,
                          outcome = spend ~ treatment + recency + history + channel, 
                          data = biased_data)




summary(spend.heckit)



# -->
# Very different estimate ...

