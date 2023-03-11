setwd("//media//kswada//MyFiles//R//gapminder")

packages <- c("dplyr", "modelr", "tidyverse", "gapminder")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gapminder
# ------------------------------------------------------------------------------

gapminder

str(gapminder)



# ----------
# basics

gapminder %>% ggplot(aes(year, lifeExp, group = country)) + geom_line(alpha = 1/3)


# --> Overall, it looks like life expectancy has been steadily improving.
# However, if you look closely, you might notice some countries that do not follow this pattern.



# ------------------------------------------------------------------------------
# Example of analysis for one country
# ------------------------------------------------------------------------------
nz <- filter(gapminder, country == "New Zealand")

nz %>% ggplot(aes(year, lifeExp)) + geom_line() + ggtitle("Full data")

nz_mod <- lm(lifeExp ~ year, data = nz)

nz %>% add_predictions(nz_mod) %>% ggplot(aes(year, pred)) + geom_line() + ggtitle("Linear trend")

nz %>% add_residuals(nz_mod) %>% ggplot(aes(year, resid)) + geom_hline(yintercept = 0, colour = "white", size = 3) + geom_line() + ggtitle("Remaining pattern")


# model quality
broom::glance(nz_mod)

