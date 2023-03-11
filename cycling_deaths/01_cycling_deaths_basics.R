setwd("//media//kswada//MyFiles//R//cycling_deaths")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  London Cycling Deaths data
#  - Aberdein and Spiegelhalter (2013) observed that from November 5-13, 2013, six people were killed while cycling in London.
#    How unusual is this number of deaths in less than a two-week period ?
#    Was this a freak occurrence, or shold Londoners petition for cycling lanes and greater road safety ?
#    To answer these questions, they obtained data from the UK Department of Transport Road Safety Data from 2005 - 2012 and
#    selected all accident fatalities of cyclists within the city of London.
#    The data, comprising 208 counts of deaths in the fortnightly periods from January 2005 to December 2012.
#
#  - It seems reasonable to assume that, in any short period of time, deaths of people riding bicycles are independent events.
#    If, in addition, the probabiliti of such events is constant over this time span, the Poisson distribution
#    should descrive the distribution of 0,1,2,3, ..., deaths.
#    Then, an answer to the main question can be given in terms of the probability of six (or more) deaths in a comparable period of time.
# ------------------------------------------------------------------------------
data("CyclingDeaths", package = "vcdExtra")

data <- CyclingDeaths

data

( tab <- table(data$deaths) )



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------
k = names(tab)

par(mfrow=c(1,1))
b <- barplot(tab, names.arg = k, xlab = "Number of deaths'", ylab = "Number of fortnights", col = "lightblue", cex.lab = 1.5)


