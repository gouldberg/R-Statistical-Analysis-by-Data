setwd("//media//kswada//MyFiles//R//nes96")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nes96
#   - Data drawn from a subset of the 1996 American National Election Study.
# ------------------------------------------------------------------------------

data("nes96", package = "faraway")

str(nes96)

head(nes96)



# ----------
party <- nes96$PID

levels(party)


# We collapse this to three
levels(party) <- c("Democrat", "Democrat", "Independent", "Independent", "Independent", "Republican", "Republican")



# The income variable in the original data was an ordered factor with income ranges.
# We have converted this to a numeric variable by taking the midpoint of each range.
inca <- c(1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115)

income <- inca[unclass(nes96$income)]



# For simplicity, we consider only the age, education level and income group of the respondents
rnes96 <- data.frame(party, income, education = nes96$educ, age = nes96$age)


car::some(rnes96)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(rnes96)



# ----------
# Relationship between party affilliation and education, age and income.

egp <- group_by(rnes96, education, party) %>% dplyr::summarise(count = n()) %>% group_by(education) %>% mutate(etotal = sum(count), proportion = count / etotal)

ggplot(egp, aes(x = education, y = proportion, group = party, linetype = party)) + geom_line()



# ----------
# Unfortunately there are relatively low counts in some income categories making the computation of a stable proportion difficult.
# To overcome this, we first group the income into 7 groups of roughly equal size and then follow much the same calculation as for education.

igp <- mutate(rnes96, incomegp = cut_number(income, 7)) %>% group_by(incomegp, party) %>% dplyr::summarise(count = n()) %>% group_by(incomegp) %>%
  mutate(etotal = sum(count), proportion = count / etotal)


ggplot(igp, aes(x = incomegp, y = proportion, group = party, linetype = party)) + geom_line()



# -->
# This is cross-sectional rather than longitudinal data, so we cannot say anything about what might happen to an individual with,
# for example, increasing income.
# We can only expect to make conclusions about the relative probability of party affiliations for different individuals with different incomes.


