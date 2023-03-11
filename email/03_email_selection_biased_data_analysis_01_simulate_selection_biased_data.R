# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  email
# ------------------------------------------------------------------------------

email <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

str(email)

dim(email)


car::some(email)



# ------------------------------------------------------------------------------
# generate biased data
# ------------------------------------------------------------------------------

# excluding Womens E-Mail
# Mens E-Mail as treatment


# RCT data
male_df <- email %>% filter(segment != "Womens E-Mail") %>% mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))




# ----------
# biased data:  more treated for potential users (history and recency)


set.seed(1)

obs_rate_c <- 0.5

obs_rate_t <- 0.5


biased_data <- male_df %>% mutate(obs_rate_c = if_else((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
                                  obs_rate_t = if_else((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
                                  random_number = runif(n = NROW(male_df))) %>% as.data.frame()

head(biased_data)


biased_data <- biased_data %>% filter((treatment == 0 & random_number < obs_rate_c) | (treatment == 1 & random_number < obs_rate_t))
                                  

head(biased_data)
