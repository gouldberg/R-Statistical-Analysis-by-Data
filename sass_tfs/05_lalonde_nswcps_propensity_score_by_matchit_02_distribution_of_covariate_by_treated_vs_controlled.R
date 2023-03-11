setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


library(ROCR)
library(lattice)



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
# Distribution of each covariates:  Treated Units vs. Controlled Units by Q-Q plot
# ------------------------------------------------------------------------------

# If the empirical distributions are the same in the treated and control groups,
# the points in the Q-Q plots would all lie on the 45 degree line

# Deviations from the 45 degree line indicate differences in the empirical distribution


plot(m_mod)




# ----------
# nswdw_data:  almost no biase for black and hispanic
plot(m_mod_list[[1]])



# cps1_nsw_data:  Treated Unit have higher black ratio compared to Controlled Unit, not so for hispanic ratio 
plot(m_mod_list[[2]])



# cps3_nsw_data:  Treated Units have higher black ratio, and lower hispanic ratio compared to Controlled Unit
plot(m_mod_list[[3]])





