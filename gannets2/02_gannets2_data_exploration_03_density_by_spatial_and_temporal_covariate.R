setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)



# ------------------------------------------------------------------------------
# Data exploration:  Gannet density vs. spatial coordinates and year
# ------------------------------------------------------------------------------
Gannets2$G     <- Gannets2$Gannets_in_transect

Gannets2$GProp <- Gannets2$G / Gannets2$Area

MyCex <- 2 *sqrt(20 * Gannets2$GProp / max(Gannets2$GProp))

xyplot(Ykm ~ Xkm | factor(Year), aspect = "iso",
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
       data = Gannets2, pch = 16, col = 1, layout = c(4,3), cex = MyCex,
       xlab = list(label = "Xkm", cex = 1.5),
       ylab = list(label = "Ykm", cex = 1.5))


# -->
# spatial patterns change over time, indicating that we may need a 3-dimensional smoother of the form f(Period, X, Y) or f(Year, X, Y).
# From a modeling standpoint, it would be best to base the decision to use a 3-dimensional smoother on the underlying biological questions,
# but software and hardware limitations introduce a data phising element.



# ------------------------------------------------------------------------------
# Data exploration:  Gannet density vs. hour, day and year
# ------------------------------------------------------------------------------
xyplot(DayInYear ~ TimeH | factor(Year),
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
       data = Gannets2,  pch = 16, col = 1, layout = c(4,3), cex = MyCex,
       xlab = list(label = "Hour", cex = 1.5),
       ylab = list(label = "Day", cex = 1.5))


# -->
# This graph illustrantes how the time of day effect changes over the days, at least during the final 2 years of sampling.
# It also shows that in 20013 we have a group of observations from a period that was not sampled in other years.
# in 2003 the data collection began 20 days earlier.
# One could argue that the 20 extra days means that we are no longer comparing like with like, and that these observations should be removed.
# We will keep them.



# ------------------------------------------------------------------------------
# Data exploration:  Cleveland dotplot of gannet abundance
# ------------------------------------------------------------------------------
par(mar = c(5,5,2,2))
plot(x = Gannets$G, y = 1:nrow(Gannets), xlab = "Gannets", ylab = "Order of the data", pch = 16,  cex = 0.8, cex.lab = 1.5)


# -->
# Note that some values are large.


