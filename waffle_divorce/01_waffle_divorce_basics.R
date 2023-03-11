setwd("//media//kswada//MyFiles//R//waffle_divorce")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Waffle Divorce
#   - One of the most reliable sources of waffles in North America, if not the entire world, is a Waffle House diner.
#     Waffle House is nearly always open, even just after a hurricane. Most diners invest in disaster preparedness,
#     including having their own electrical generators. As a consequence, the United States' disaster relief agency (FEMA) informally uses
#     Waffle House as an index of disaster severity. If the Waffle House is closed, that's a serious event.
#   - It is ironic then that steadfast Waffle House is associated with the nation's highest divorce rates. States with many Waffle Houses per person, 
#     like Georgia and Alabama, also have some of the highest divorce rates in the United States.
#     The lowest divorce rates are found where there are zero Waffle Houses. Could always-available waffles and hash brown potatoes put marriage at risk ?
#   - Probably not. This is an example of a misleading correlation.  Waffle House began in Georgia in the year 1955. Over time, the diners spread across
#     the Southern United States, remaining largely bounded within it. So Waffle House is associated with the South. Divorce is not a uniquely Sourthern
#     institution, but is more common anyplace that people marry young, and many communities in the South still frown on youn people "shacking up" and
#     living together out of wedlock. So it's probably just an accident of history that Waffle House and high divorce rates both occur in the South.
#   - Variables
#        - Location : State name
#        - Loc : State abbreviation
#        - Population : 2010 population in millions
#        - MedianAgeMarriage: 2005-2010 median age at marriage
#        - Marriage : 2009 marriage rate per 1000 adults
#        - Marriage.SE : Standard error of rate
#        - Divorce : 2009 divorce rate per 1000 adults
#        - Divorce.SE : Standard error of rate
#        - WaffleHouses : Number of diners
#        - South : 1 indicates Southern State
#        - Slaves1860 : Number of slaves in 1860 census
#        - Population1860 : Population from 1860 census
#        - PropSlaves1860 : Proportion of total population that were slaves in 1860
# ------------------------------------------------------------------------------

data("WaffleDivorce", package = "rethinking")

d <- WaffleDivorce

dim(d)

str(d)



# ------------------------------------------------------------------------------
# plot Divorce rate ~ Median age marriage and its measurement error
# ------------------------------------------------------------------------------
# In this data, both the divorce rate variable and the marriage rate variabe are measured with substantial error,
# and that error is reported in the form of standard errors.
# Importantly, the amount of error varies a lot across States.

plot(d$Divorce ~ d$MedianAgeMarriage, ylim = c(4, 15), xlab = "Median age marriage", ylab = "Divorce rate")

for(i in 1:nrow(d)){
  ci <- d$Divorce[i] + c(-1, 1) * d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines(c(x,x), ci)
}



# ------------------------------------------------------------------------------
# plot Divorce rate ~ log population with and its measurement error
# ------------------------------------------------------------------------------
# Large States provide better samples, so their measurement error is smaller.

plot(d$Divorce ~ log(d$Population), ylim = c(4, 15), xlab = "log population", ylab = "Divorce rate")

for(i in 1:nrow(d)){
  ci <- d$Divorce[i] + c(-1, 1) * d$Divorce.SE[i]
  x <- log(d$Population)[i]
  lines(c(x,x), ci)
}




