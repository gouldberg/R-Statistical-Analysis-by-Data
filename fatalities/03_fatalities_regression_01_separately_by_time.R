setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)




# ------------------------------------------------------------------------------
# cross-sectional analysis separately by year
# ------------------------------------------------------------------------------


fm <- frate ~ beertax


mod82 <- lm(fm, Fatalities, subset = year == 1982)


mod88 <- update(mod82, subset = year == 1988)



# ----------
summary(mod82)


summary(mod88)



# ----------
stargazer::stargazer(mod82, mod88, intercept.bottom = FALSE, type = "text")




# ----------
# employing coeftest for compactness
lmtest::coeftest(mod82)


lmtest::coeftest(mod88)



# -->
# Surprisingly, the coefficient is significant and positive !!

