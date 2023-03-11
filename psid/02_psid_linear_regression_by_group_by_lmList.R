setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# Fit linear model by each person by lmList()
# ------------------------------------------------------------------------------

library(lme4)


# We now fit a line for all the subjects and plot the results.
# The lmList command fits a linear model to each group within the data, here specified by person
# A list of linear models, one for each group, is returned from which we extract the intercepts and slopes

ml <- lmList(log(income) ~ I(year - 78) | person, psid)


head(summary(ml))




# ----------
# extract intercepts and slopes
intercepts <- sapply(ml, coef)[1,]

slopes <- sapply(ml, coef)[2,]


df <- cbind(psid %>% dplyr::select(age, educ, sex, person) %>% unique() %>% arrange(person), intercepts, slopes)


# also adding education levels
df$edulevel <- cut(df$educ, c(0, 8.5, 12.5, 20), labels = c("lessHS", "HS", "moreHS"))

df

