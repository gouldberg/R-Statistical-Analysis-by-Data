setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# Plot the fitted distribution parameters
# ------------------------------------------------------------------------------

fittedPlot(m0, x = dbbmi1$age)




# ------------------------------------------------------------------------------
# Plot the fitted distribution parameters of multiple models simultanesously
# ------------------------------------------------------------------------------

dbbmi1$Tage <- (dbbmi1$age) ^ (m0$power)

m1 <- gamlss(bmi ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), tau.fo = ~pb(Tage), family = BCPEo, data = dbbmi1)



# ----------
fittedPlot(m1, m0, x = dbbmi1$age, line.type = c(1,2))


# -->
# Very similar for the two models