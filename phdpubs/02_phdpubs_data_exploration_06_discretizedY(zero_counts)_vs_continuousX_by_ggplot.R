setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# data exploration:  discretized Y (zero counts probability) vs. continuous X by ggplot
# ------------------------------------------------------------------------------


library(ggplot2)


PhdPubs2 <- PhdPubs %>% mutate(prevalence = ifelse(articles > 0, 1, 0))


ggplot(PhdPubs2, aes(x = mentor, y = as.numeric(prevalence) - 1)) + geom_jitter(position = position_jitter(height = 0.05), alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  stat_smooth(method = "loess", color = "red", fill = "red", size = 1.5) + labs(y = "prevalence")


