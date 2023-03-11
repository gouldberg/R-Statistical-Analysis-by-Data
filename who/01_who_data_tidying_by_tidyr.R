setwd("//media//kswada//MyFiles//R//who")

library(dplyr)
library(tidyr)



# ---------------------------------------------------------------------------
# data: Who
#  - Contains tuberculosis (TB) cases broken down by year, country, age, gender, and diagnosis method.
#  - The data comes from the 2014 World Health Organization Gloabl Tuberculosis Report,
#    available at http://www.who.int/tb/country/data/download/en/.
# ---------------------------------------------------------------------------
who

str(who)

print(object.size(who), units="auto")



# ---------------------------------------------------------------------------
# The best place to start is almost always to gather together the columns that are not variables
# ---------------------------------------------------------------------------
who1 <- who %>% gather(
  new_sp_m014:newrel_f65,
  key = "key",
  value = "cases",
  na.rm = TRUE
)


who1

print(object.size(who1), units="auto")



# ---------------------------------------------------------------------------
# Get some hint of the structure of the values in the new kye column by counting them
# ---------------------------------------------------------------------------
who1 %>% count(key) %>% print(n=100)


# -->
# the 1st 3 letters:  denote whether the column contains new or old cases of TB
# rel stands for cases of relapse
# ep stands for cases of extrapulmonary TB
# sn stands for cases of pulmonary TB that could not be diagnosed by a pulmonary smear (smear negative)
# sp stands for cases of pulmonary TB that could be diagnosed by a pulmonary smear (smear positive)
# the 6th letter give the sex of TB patients
# the remaining numbers give the agegroup: 013 = 0-14 years old, 65 = 65 or older



# ---------------------------------------------------------------------------
# replace the key value character "newrel" with "new_rel"
# ---------------------------------------------------------------------------
# str_replace preserves not-matched value as-is
who2 <- who1 %>% mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who2

print(object.size(who2), units="auto")



# ---------------------------------------------------------------------------
# separate the values in each code with passes of separate()
# ---------------------------------------------------------------------------
who3 <- who2 %>% separate(key, c("new", "type", "sexage"), sep = "_")

who3

print(object.size(who3), units="auto")



# ---------------------------------------------------------------------------
# drop the new column because it's constant in this dataset
# also drop iso2 and iso3 since they're redundant
# ---------------------------------------------------------------------------
who3 %>% count(new)
who3 %>% count(country, iso2)
who3 %>% count(country, iso2) %>% distinct()
who3 %>% count(country, iso3)
who3 %>% count(country, iso3) %>% distinct()


who4 <- who3 %>% dplyr::select(-new, -iso2, -iso3)

who4

print(object.size(who4), units="auto")



# ---------------------------------------------------------------------------
# separate sexage into sex and age by splitting after the 1st character
# ---------------------------------------------------------------------------
who5 <- who4 %>% separate(sexage, c("sex", "age"), sep = 1)

who5

print(object.size(who5), units="auto")





