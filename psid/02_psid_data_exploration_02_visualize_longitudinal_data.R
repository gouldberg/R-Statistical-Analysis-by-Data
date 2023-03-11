setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# data exploration:  visualize longitudinal data  by person along year
# ------------------------------------------------------------------------------

psid20 <- filter(psid, person <= 20)



# ----------
library(ggplot2)

ggplot(psid20, aes(x = year, y = income)) + geom_line() + facet_wrap(~ person)



# -->
# We see that some individuals have a slowly increasing income, typical of someone in steady employment in the same job.
# Other individuals have more erratic incomes.



# ------------------------------------------------------------------------------
# data exploration:  visualize longitudinal data  by person and group along year
# ------------------------------------------------------------------------------

# We can also show how the incomes vary by sex. Income is more naturally considered on a log-scale.
# We added $100 to the income of each subject to remove the effect of some subjects having very low incomes for short periods of time.

# ggplot(psid20, aes(x = year, y = income, group = person)) + geom_line() + facet_wrap(~ sex) + scale_y_log10()


ggplot(psid20, aes(x = year, y = income + 100, group = person)) + geom_line() + facet_wrap(~ sex) + scale_y_log10()



# -->
# We see that men's incomes are generally higher and less variable while women's incomes are more variable, but are perhaps increasing more quickly.



# ----------
# We can fit a line to each subject starting with the first
# We have centered the predictor at the median value so that the intercept will represent the predicted log income in 1978 and not the year 1900
# which would be nonsense.
psid[psid$person == 1,"year"]

lmod <- lm(log(income) ~ I(year - 78), subset = (person == 1), psid)

summary(lmod)



