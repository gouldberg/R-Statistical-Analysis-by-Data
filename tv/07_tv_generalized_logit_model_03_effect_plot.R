setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "lmtest", "ggplot2", "directlabels", "effects", "car", "nnet")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)



# ------------------------------------------------------------------------------
# Convert 3-way table to frequency form
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)

car::some(TV.df)



# ----------
levels(TV.df$Network)


# choose "ABC" clearly as baseline category
TV.df$Network <- relevel(TV.df$Network, ref = "ABC")

levels(TV.df$Network)




# ------------------------------------------------------------------------------
# Effect plot for the probabilities of Network viewers
# ------------------------------------------------------------------------------

# main effect plot
plot(effects::allEffects(tv.multinom))



# ----------
# Stacked effect model
# It is analogous to the full-model plot shown above

plot(effects::Effect(c("Day"), tv.multinom), style = "stacked", key.args = list(x = .05, y = .9))

plot(effects::Effect(c("Time"), tv.multinom), style = "stacked", key.args = list(x = .05, y = .9))

plot(effects::Effect(c("Day", "Time"), tv.multinom), style = "stacked", key.args = list(x = .05, y = .9))



# -->
# Consistent with 3-way CA analysis
# Thursday, especially NBC 10:00 Time
# Monday, especially CBS 8:00 Time


# Differenct from 3-way CA analysis
# CA analysis: Tuesday, especially ABC 9:00 Time
# Generalized Logit Model:  Not only Tuesday, but also Wednesday and Friday 9:00 Time for ABC


