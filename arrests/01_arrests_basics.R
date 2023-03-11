setwd("//media//kswada//MyFiles//R//arrests")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra", "carData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arrests
#  - In the summer of 2002, the Toronto Star newspaper launched an investigation on the topic of possible racial profiling by the Toronto police service.
#    Through freedom of information requests, they obtained a data base of over 600,000 arrest records on all potential charges in the period from 1996-2002,
#    the largest data bases on crime arrests and disposition ever assembled in Canada.
#  - In order to examine the issue of racial profiling (different treatment as a funciton of race) they excluded all charges such as assault, robbery, speeding,
#    and driving under the influence, where the police have no discretion regarding the laying of a charge.
#    They focused instead on a subset of arrests, where the police had various options.
#  - Among these, for people arrested for a single charge of simple possession of a small amount of marijuana, police have the option of releasing the arrestee,
#    with a summons ("Form 9") to appear in court (similar to a parking ticket), or else the person could be given harsher treatment -- brought to a police station
#    or held in jail for a bail hearing ("Show cause"). The main question for the Toronto Star was whether the subject's skin color had any influence on the
#    likelihood that the person would be released with a summons.
#  - Their results, published in a week-long series of articles in December 2002, concluded that there was strong evidence that black and white subjects were
#    treated differently. An important part of the analysis and the public debate that ensued was to show that other variables that might account for these
#    differences had been controlled or adjusted for.
#
#  - The data set Arrests gives a simplified version of the Star database, containing records for 5,226 cases of arrest on the charge of simple possession of marijuana
#    analyzed by the newspaper.
#
#  - "checks": When someone is stopped by police, his/her name is checked in six police data bases that record previous arrests, convictions, whether on parole, etc.
#    The variable "checks" records the number 0-6, in which the person's name appeared.
# ------------------------------------------------------------------------------
data("Arrests", package = "carData")

dim(Arrests)

str(Arrests)


car::some(Arrests)


data <- Arrests



# To allow for possible nonlinear effects of year, this variable was treated as a factor rather than as a (linear) numeric variable
data$year <- as.factor(data$year)
