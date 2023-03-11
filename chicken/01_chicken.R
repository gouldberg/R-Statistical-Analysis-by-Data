setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr", "astsa", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chiken
# ------------------------------------------------------------------------------


data("chicken", package = "astsa")


str(chicken)


car::some(chicken)




# ------------------------------------------------------------------------------
# data exploration   time series plot
# ------------------------------------------------------------------------------


MTSplot(chicken)






# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------


forecast::ndiffs(chicken)


plot(diff(chicken), type = "l")


acf2(diff(chicken))


sarima(chicken, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)



mvspec(diff(chicken))


mvspec(diff(chicken), spans = c(3,3))


spec.ar(diff(chicken), log = "no", method = "yule-walker")


spec.ar(diff(chicken), log = "no", method = "burg")
