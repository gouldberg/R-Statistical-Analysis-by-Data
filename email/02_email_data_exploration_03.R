setwd("//media//kswada//MyFiles//R//email")

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
# Conversion rate comparison
# ------------------------------------------------------------------------------

# excluding Womens E-Mail
# Mens E-Mail as treatment

male_df <- email %>% filter(segment != "Womens E-Mail") %>% mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))


car::some(male_df)



# ----------
summary_by_segment <- male_df %>% group_by(treatment) %>% dplyr::summarise(conversion_rate = mean(conversion), spend_men = mean(spend), count = n())


summary_by_segment



# -->
# higher conversion for treatement ("Mens E-Mail")



# ------------------------------------------------------------------------------
# t.test for spend among treatment or non-treatment group
# ------------------------------------------------------------------------------


mens_mail <- male_df %>% filter(treatment == 1) %>% pull(spend)

no_mail <- male_df %>% filter(treatment == 0) %>% pull(spend)



# ----------
boxplot(spend ~ treatment, data = male_df)


car::densityPlot(~ spend, g = as.factor(male_df$treatment), data = male_df)



t.test(mens_mail, no_mail, var.equal = FALSE)



