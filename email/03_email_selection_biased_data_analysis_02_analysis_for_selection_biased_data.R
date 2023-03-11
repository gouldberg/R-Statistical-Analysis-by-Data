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
# Biased data analysis:  conversion rate
# ------------------------------------------------------------------------------


summary_by_segment_biased <- biased_data %>% group_by(treatment) %>% dplyr::summarise(conversion_rate = mean(conversion), spend_men = mean(spend), count = n())


summary_by_segment_biased



# -->
# higher conversion for treatement ("Mens E-Mail")



# ------------------------------------------------------------------------------
# Biased data analysis:  t.test for spend
# ------------------------------------------------------------------------------


# mean difference

mens_mail_biased <- biased_data %>% filter(treatment == 1) %>% pull(spend)

no_mail_biased <- biased_data %>% filter(treatment == 0) %>% pull(spend)


mean(mens_mail_biased)

mean(no_mail_biased)

mean(mens_mail_biased) - mean(no_mail_biased)



# -->
# simple mean difference is 1.528 - 0.548 = 0.979




# ----------
# t.test for spend among treatment or non-treatment group


t.test(mens_mail_biased, no_mail_biased, var.equal = FALSE)


