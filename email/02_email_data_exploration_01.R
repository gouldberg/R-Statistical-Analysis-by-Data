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
# data exploration:  data distribution by each variable
# ------------------------------------------------------------------------------


summary(email)


Hmisc::describe(email)

