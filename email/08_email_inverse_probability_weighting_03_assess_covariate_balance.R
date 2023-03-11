setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_ori, ipw_mod_biased)

ipw_mod_name <- c("ipw_mod_ori", "ipw_mod_biased")



# select model
ipw_mod <- ipw_mod_list[[1]]

ipw_mod <- ipw_mod_list[[2]]




# ------------------------------------------------------------------------------
# Assess covariate balance
# ------------------------------------------------------------------------------

library(cobalt)


# Standardized mean difference between Treated and Controlled of each covariate
# Adjusted:  after matching
# Unadjusted:  raw data


love.plot(ipw_mod, stats = "mean.diffs", threshold = 0.1, stars = "std", size = 5, col = c(gray(0.7), "blue"))




# ----------
bal.tab(m_mod)



