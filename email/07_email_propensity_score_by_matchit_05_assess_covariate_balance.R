setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

m_mod_list <- list(m_mod_ori, m_mod_biased_data)

m_mod_name <- c("m_mod_ori", "m_mod_biased_data")



# select model
m_mod <- m_mod_list[[1]]

m_mod <- m_mod_list[[2]]





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




# ----------
# for "history"

tmp0 <- biased_data %>% filter(treatment == 0)
tmp1 <- biased_data %>% filter(treatment == 1)
mean0 <- mean(tmp0$history)
mean1 <- mean(tmp1$history)

tmp0_m <- matched_data_biased %>% filter(treatment == 0)
tmp1_m <- matched_data_biased %>% filter(treatment == 1)
mean0_m <- mean(tmp0_m$history)
mean1_m <- mean(tmp1_m$history)


( mean1 - mean0 ) / sd(biased_data$history)

mean1_m / sd(tmp1_m$history) - mean0_m / sd(tmp0_m$history)




# ----------
# for "recency"

tmp0 <- biased_data %>% filter(treatment == 0)
tmp1 <- biased_data %>% filter(treatment == 1)
mean0 <- mean(tmp0$recency)
mean1 <- mean(tmp1$recency)

tmp0_m <- matched_data_biased %>% filter(treatment == 0)
tmp1_m <- matched_data_biased %>% filter(treatment == 1)
mean0_m <- mean(tmp0_m$recency)
mean1_m <- mean(tmp1_m$recency)


( mean1 - mean0 ) / sd(biased_data$recency)

mean1_m / sd(tmp1_m$history) - mean0_m / sd(tmp0_m$history)




# ----------
# for "channel = Multichannel"

tmp0 <- biased_data %>% filter(treatment == 0)
tmp1 <- biased_data %>% filter(treatment == 1)
mean0 <- mean(tmp0$channel == "Multichannel")
mean1 <- mean(tmp1$channel == "Multichannel")

tmp0_m <- matched_data_biased %>% filter(treatment == 0)
tmp1_m <- matched_data_biased %>% filter(treatment == 1)
mean0_m <- mean(tmp0_m$channel == "Multichannel")
mean1_m <- mean(tmp1_m$channel == "Multichannel")


mean1 - mean0

mean1_m - mean0_m




# ----------
# for reference:  balanceTable_init

balanceTable_init


