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
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

squid.fitp <- cbind(Squid, weight_pred = predict(mod_obj, type = "response"))


head(squid.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(squid.fitp, aes(x = DML, y = weight_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ fMONTH)

