setwd("//media//kswada//MyFiles//R//titanic")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Titanic
# ------------------------------------------------------------------------------
data <- Titanic

data



# ------------------------------------------------------------------------------
# doubledecker()
# ------------------------------------------------------------------------------
# The levels of the response (Survived) are shaded in increasing grey levels, highlighting the proportions of survival
doubledecker(Survived ~ Class + Age + Sex, data = data)



# --> This order of variables makes it easiest to compare survival of men and women within each age-class combination,
# but you can also see that survival of adult women decreases with class, and survival among men was greatest in first class.





