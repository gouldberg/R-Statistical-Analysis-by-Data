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
# Nested data structure:  group and nest the dataframe
# ------------------------------------------------------------------------------
by_country <- gapminder %>% group_by(country, continent) %>% nest()

by_country


# --> This creates a data frame that has one row per group (per country), 
# and a rather unusual column: data.data is a list of data frames (or tibbles, to be precise)


# ----------
# access each data frame by [[]]
by_country$data[[1]]



# ------------------------------------------------------------------------------
# model by country
# ------------------------------------------------------------------------------
# model fitting function
country_model <- function(df){ lm(lifeExp ~ year, data = df) }



# ----------
models <- map(by_country$data, country_model)

by_country <- by_country %>% mutate(model = map(data, country_model))

by_country



# ----------
by_country %>% filter(continent == "Europe")



# ----------
by_country %>% arrange(continent, country)



# ------------------------------------------------------------------------------
# caluclate residuals by country and check
# ------------------------------------------------------------------------------

by_country <- by_country %>% mutate(resids = map2(data, model, add_residuals))

by_country



# ----------
# Turn the list of data frmaes back into regular data frame by unnest()
resids <- unnest(by_country, resids)

resids



# ----------
# geom_smooth() using method = 'gam'
resids %>% ggplot(aes(year, resid)) + geom_line(aes(group = country), alpha = 1/3) + geom_smooth(se = FALSE)



# ----------
resids %>% ggplot(aes(year, resid, group = country)) + geom_line(alpha = 1/3) + facet_wrap(~continent)


# --> It looks like we've missed some mild pattern.
# There's also something interesting going on in Africa.



# ------------------------------------------------------------------------------
# model quality
#  - broom::glance(model):  returns a row for each model. Each column give a model summary (either a measure of model quality or complexity, or a combination of the two)
#  - broom::tidy(model):  returns a row for each coefficient in the model. Each column gives information about the estimate or its variability
#  - broom::augment(model, data):  returns a row for each row in data, adding extra values like residuals, and influence statistics
# ------------------------------------------------------------------------------
glance <- by_country %>% mutate(glance = map(model, broom::glance)) %>% unnest(glance, .drop = TRUE)
aug <- by_country %>% mutate(aug = map(model, broom::augment)) %>% unnest(aug, .drop = TRUE)


glance

aug


glance %>% arrange(r.squared)



# ----------
# plot r.squared by continent adding jitter
glance %>% ggplot(aes(continent, r.squared)) + geom_jitter(width = 0.5)



# ----------
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% semi_join(bad_fit, by = "country") %>% ggplot(aes(year, lifeExp, color = country)) + geom_line()




