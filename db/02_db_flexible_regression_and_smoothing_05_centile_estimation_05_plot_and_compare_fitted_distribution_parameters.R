setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# Plot the fitted distribution parameters
# ------------------------------------------------------------------------------

fittedPlot(m0_c, x = db$age)




# ------------------------------------------------------------------------------
# Plot the fitted distribution parameters of multiple models simultanesously
# ------------------------------------------------------------------------------

db$Tage <- (db$age) ^ (m0_c$power)

m01 <- gamlss(head ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), tau.fo = ~pb(Tage), family = BCCGo, trace = FALSE, data = db)



# ----------
fittedPlot(m01, m0_c, x = db$age, line.type = c(1,2))


