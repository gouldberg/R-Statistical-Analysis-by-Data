setwd("//media//kswada//MyFiles//R//titanic")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Titanic
# ------------------------------------------------------------------------------
data <- Titanic

data


data2 <- data[, , 2:1, 2:1]

data2["Crew", , "Child", ] <- NA

data2



# ------------------------------------------------------------------------------
# Examine the odds ratios for
#  - Survived and Age by Class, Sex
#  - Survived and Sex by Class, Age
# ------------------------------------------------------------------------------
# loddsratio() in vcd package calculates log odds ratios for the categories of the 1st two dimensions of an n-way table,
# together with their asymptotic covariance matrix.
( titanic_lor1 <- loddsratio(~ Survived + Age | Class + Sex, data = data2) )

titanic_lor1


( titanic_lor2 <- loddsratio(~ Survived + Sex | Class + Age, data = data2) )

titanic_lor2



# the plot method for loddsratio objects convenierntly plots the log odds ratio (LOR) against the strata variables
# by default also adds error bars.
plot(titanic_lor1)

plot(titanic_lor2)



# --> The odds ratio of survival for adults relative to children was always greater for females as compared to males, but much less so in 3rd class.
# And the odds ratio of survival for maels versus femaels was always greater for children than adults, again less so in 3rd class.
