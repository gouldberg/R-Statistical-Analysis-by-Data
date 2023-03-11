setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_nswdw_1, ipw_mod_cps1nsw_1, ipw_mod_cps3nsw_1, ipw_mod_nswdw_2, ipw_mod_cps1nsw_2, ipw_mod_cps3nsw_2)

ipw_mod_name <- c("ipw_mod_nswdw_1", "ipw_mod_cps1nsw_1", "ipw_mod_cps3nsw_1", "ipw_mod_nswdw_2", "ipw_mod_cps1nsw_2", "ipw_mod_cps3nsw_2")



# select model
ipw_mod <- ipw_mod_list[[1]]

ipw_mod <- ipw_mod_list[[2]]

ipw_mod <- ipw_mod_list[[3]]

ipw_mod <- ipw_mod_list[[4]]

ipw_mod <- ipw_mod_list[[5]]

ipw_mod <- ipw_mod_list[[6]]




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




# not balanced ...

