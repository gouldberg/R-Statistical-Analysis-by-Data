setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

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
# Distribution of propensity scores: jitter plot
# ------------------------------------------------------------------------------

# The jitter plot shows the overall distribution of propensity scores in treated and control groups
# plot(m_mod, type = "jitter")





# ------------------------------------------------------------------------------
# Distribution of propensity scores
# ------------------------------------------------------------------------------


# The histogram of distributions of propensity score
plot(m_mod, type = "hist", col = gray(0.7))



# for compariton, propensity score estimated by glm logistic regression
# Note that controlled include unmatched unit

lattice::histogram(~ pred_prob | treatment, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 20))



