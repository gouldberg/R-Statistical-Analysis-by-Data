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
# data exploration
# ------------------------------------------------------------------------------

library(tidyverse)


# ----------
# Sales in before (1970 - 1987) and after (1988 -) in California and other US states

Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales * pop16) / sum(pop16)) %>%
  spread(state, sales)
  


Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = period,
             shape = state,
             linetype = state)) +
  geom_point(size = 2) +
  geom_line(aes(group = state)) +
  ylim(0, NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) +
  scale_x_discrete(name ="Period",limits=c("before","after"))



# ----------
# time series trend of cigarette sales
Cigar %>%
  mutate(state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(year,state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = year,
             shape = state,
             linetype = state)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 88, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"))

