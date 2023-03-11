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
# Distribution of propensity scores: jitter plot
# ------------------------------------------------------------------------------

# The jitter plot shows the overall distribution of propensity scores in treated and control groups
# plot(m_mod, type = "jitter")





# ------------------------------------------------------------------------------
# Distribution of propensity scores
# ------------------------------------------------------------------------------


# The histogram of distributions of propensity score
# Note that scale is different in each histogram

plot(m_mod, type = "hist", col = gray(0.7))


