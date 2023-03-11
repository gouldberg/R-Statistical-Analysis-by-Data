setwd("//media//kswada//MyFiles//R//cigar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cigar
# ------------------------------------------------------------------------------

data("Cigar", package = "Ecdat")

dim(Cigar)

str(Cigar)


car::some(Cigar)


# ----------
# states To be excluded since tax for cigaret is increased by 50 cents or more from 1988.
# If those states are included, the assumption of Common Trend does not hold.
# Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3, 9, 10, 22, 21, 23, 31, 33, 48)


Cigar <- Cigar %>% filter(!state %in% skip_state, year >= 70) %>% mutate(area = if_else(state == 5, "CA", "Rest of US"))



# ------------------------------------------------------------------------------
# Estimate difference-in-differences by joint regressions
# ------------------------------------------------------------------------------

Cigar_did_sum <- Cigar %>% mutate(post = if_else(year > 87, 1, 0),
                                  ca = if_else(state == 5, 1, 0),
                                  state = factor(state),
                                  year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>% 
  summarise(sales = sum(sales * pop16) / sum(pop16))


Cigar_did_sum



# ----------
library(broom)

Cigar_did_sum_reg <- Cigar_did_sum %>% lm(data = ., sales ~ ca + post + ca:post + year_dummy) %>% tidy() %>% 
  filter(!str_detect(term, "state"), !str_detect(term, "year"))




# ----------
Cigar_did_sum_logreg <- Cigar_did_sum %>% lm(data = ., log(sales) ~ ca + post + ca:post + year_dummy) %>% tidy() %>% 
  filter(!str_detect(term, "state"), !str_detect(term, "year"))



Cigar_did_sum_reg

Cigar_did_sum_logreg



# -->
# Proposition99 decreased the number of cigaret boxes sold per person by 20 and 25%


