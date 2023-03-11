# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)


# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot by group
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications
gg <- ggplot(Squid, aes(DML, Testisweight)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Testisweight", x = "DML")


gg + facet_wrap(~ fMONTH)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

formula = Testisweight ~ DML | fMONTH

coplot(formula, data = Squid, ylab = "Testisweight", xlab = "DML", las=1)

