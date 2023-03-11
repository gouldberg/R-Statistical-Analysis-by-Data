setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# predictAll() function
#   - produce predictors for the current or a new data set for all the distribution parameters of a gamlss object.
#   - By default in predictAll() the predicted values of parameters (type = "response") are obtained, unlike the predict() function whose default is the 
#     predicted values fo the predictors (type = "link")
# ------------------------------------------------------------------------------


h <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), family = TF, data = abdom)

hall <- predictAll(h)



# ----------
# Fitted (predicted) values for the parameters of the distribution (TF)
hall$mu
hall$sigma
hall$nu



# ----------
# dependent variable
hall$y



# ----------
# Fitted values of all the parameters can also be saved for new data cases, e.g. for gestational age x = 20, 25, 30 weeks
newabdom <- data.frame(x = c(20, 25, 30))

hall2 <- predictAll(h, newdata = newabdom)



# ----------
# Fitted distribution of the response variable can be plotted using the pdf for each of the 3 new cases
pdf.plot(mu = hall2$mu, sigma = hall2$sigma, nu = hall2$nu, family = TF, min = 100, max = 350, step = 1)




