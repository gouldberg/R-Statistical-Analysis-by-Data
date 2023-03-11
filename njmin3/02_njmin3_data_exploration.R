setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  njmin3
# ------------------------------------------------------------------------------

data("njmin3", package = "POE5Rdata")

dim(njmin3)

str(njmin3)


car::some(njmin3)



# ------------------------------------------------------------------------------
# data exploration:  distribution of full-time equivalent employment by nj and d
# ------------------------------------------------------------------------------

psych::describe(njmin3)


Hmisc::describe(njmin3)


quantile(njmin3$fte, c(0.75, 0.90, 0.95), na.rm = TRUE)

summary(njmin3$fte)



# -->
# There are some missing values in "fte"  (26 out of 794)



# ----------
# distribution of fte
lattice::bwplot(fte ~ as.factor(d) | as.factor(nj), data = njmin3)

lattice::bwplot(log(fte) ~ as.factor(d) | as.factor(nj), data = njmin3)



# ----------
with(njmin3, by(fte, d, summary))

with(njmin3, by(fte, nj, summary))


# -->
# IT does not seem that there is some effect to full-time equivalent employment by the change of minimum wage change



# ------------------------------------------------------------------------------
# Other variables distribution difference between New Jersey and Pennsylvania
# ------------------------------------------------------------------------------

tab <- xtabs(~ bk + kfc + roys + wendys + co_owned + centralj + southj + pa1 + pa2 + nj, data = njmin3)

tab

dim(tab)



# ----------
library(vcd);  library(vcdExtra);

pairs(tab, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))


# -->
# "nj" is NO independent from "central", "southj", "pa1", "pa2"
# but almost independet from "kfc", "roys", "wendy", "co_owned"



# ----------
lattice::bwplot(demp ~ as.factor(nj), data = njmin3)

with(njmin3, by(demp, as.factor(nj), summary))


# -->
# some difference in demp between New Jersey and Pennsylvania


