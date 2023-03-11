setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Heckman's Sample Selection Model
# ------------------------------------------------------------------------------


library(sampleSelection)


re78.heckit <- selection(selection = treat ~ re74 + age + education + black + hispanic + nodegree + I(re74^2),
                          outcome = re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married + I(re74^2) + I(re75^2), 
                          data = cps3_nsw_data)




summary(re78.heckit)



# -->
# Very different estimate ...

