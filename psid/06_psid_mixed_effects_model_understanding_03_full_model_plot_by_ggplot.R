setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------
data("psid", package = "faraway")

str(psid)

car::some(psid)



# ----------
psid$cyear <- psid$year - 78

mod_obj <- lmer(log(income) ~ cyear * sex + age + educ + (cyear | person), data = psid)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

# psid.fitp <- cbind(psid, inc_pred = predict(mod_obj, type = "response"))

psid.fitp <- fortify(mod_obj)

head(psid.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(psid.fitp, aes(x = cyear, y = .fitted)) + theme_bw() + geom_point(colour = "black", size = 0.8)

gg + facet_grid(~ educ)

