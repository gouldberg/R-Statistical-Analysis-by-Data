setwd("//media//kswada//MyFiles//R//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------
Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


Squid$fMONTH = factor(Squid$MONTH)



# ----------
vf4 <- varPower(form = ~ DML | fMONTH)
M.gls4 <- gls(Testisweight ~ DML * fMONTH, data = Squid, weights = vf4)
summary(M.gls4)

mod_obj <- M.gls4



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("DML", "fMONTH"), mod_obj), 
#     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))


