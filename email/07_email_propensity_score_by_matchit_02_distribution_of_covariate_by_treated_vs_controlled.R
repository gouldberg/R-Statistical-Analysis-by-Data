# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

m_mod_list <- list(m_mod_ori, m_mod_biased)

m_mod_name <- c("m_mod_ori", "m_mod_biased")



# select model
m_mod <- m_mod_list[[1]]

m_mod <- m_mod_list[[2]]




# ------------------------------------------------------------------------------
# Distribution of each covariates:  Treated Units vs. Controlled Units by Q-Q plot
# ------------------------------------------------------------------------------

# If the empirical distributions are the same in the treated and control groups,
# the points in the Q-Q plots would all lie on the 45 degree line

# Deviations from the 45 degree line indicate differences in the empirical distribution


plot(m_mod)




# ----------
# male_df
plot(m_mod_list[[1]])



# biased_data
plot(m_mod_list[[2]])


