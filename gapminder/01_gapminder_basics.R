setwd("//media//kswada//MyFiles//R//gapminder")

packages <- c("dplyr", "modelr", "tidyverse", "gapminder")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gapminder
#  - This data was popularlized by Hans Rosling, a Swedish doctor and statistician.
#  - This data summarizes the progression of countries over time, looking at statistics like life expectancy and GDP.
#  - In this case study, we're going to focus on just three variables to answer the question:
#      - "How does life expectancy (lifeExp) change over time (year) for each country (country)?"
# ------------------------------------------------------------------------------

gapminder

str(gapminder)



# ----------
# basics

gapminder %>% ggplot(aes(year, lifeExp, group = country)) + geom_line(alpha = 1/3)


# --> Overall, it looks like life expectancy has been steadily improving.
# However, if you look closely, you might notice some countries that do not follow this pattern.

