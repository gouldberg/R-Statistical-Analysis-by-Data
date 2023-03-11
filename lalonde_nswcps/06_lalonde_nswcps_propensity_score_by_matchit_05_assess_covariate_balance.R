setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

m_mod_list <- list(m_mod_nswdw_1, m_mod_cps1nsw_1, m_mod_cps3nsw_1, m_mod_nswdw_2, m_mod_cps1nsw_2, m_mod_cps3nsw_2)

m_mod_name <- c("m_mod_nswdw_1", "m_mod_cps1nsw_1", "m_mod_cps3nsw_1", "m_mod_nswdw_2", "m_mod_cps1nsw_2", "m_mod_cps3nsw_2")



# select model
m_mod <- m_mod_list[[1]]

m_mod <- m_mod_list[[2]]

m_mod <- m_mod_list[[3]]

m_mod <- m_mod_list[[4]]

m_mod <- m_mod_list[[5]]

m_mod <- m_mod_list[[6]]




# ------------------------------------------------------------------------------
# Assess covariate balance
# ------------------------------------------------------------------------------

library(cobalt)


# Standardized mean difference between Treated and Controlled of each covariate
# Adjusted:  after matching
# Unadjusted:  raw data


love.plot(m_mod, stats = "mean.diffs", threshold = 0.1, stars = "std", size = 5, col = c(gray(0.7), "blue"))




# ----------
bal.tab(m_mod)



# -->
# mean difference / its standard deviations < 0 for matched data
# all covariate < 0.1



