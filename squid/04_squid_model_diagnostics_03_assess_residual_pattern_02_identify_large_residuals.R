# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ----------
M1 <- lm(Testisweight ~ DML * fMONTH, data = Squid)

mod_obj <- M1



# ------------------------------------------------------------------------------
# Standardized residuals by index
# ------------------------------------------------------------------------------

# standardized residuals
stand.resid <- rstandard(mod_obj, type = "sd.1")



# ----------
par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))


