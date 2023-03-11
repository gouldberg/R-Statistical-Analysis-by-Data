setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)


attenu <- attenu %>% mutate(mag2 = ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))



# ----------
mmod1 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)
mmod2 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | event), data = attenu)


mod_obj <- mmod2



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

# att.fitp <- cbind(attenu, acc_pred = predict(mod_obj, type = "response"))

att.fitp <- fortify(mod_obj)

head(att.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(att.fitp, aes(x = log(dist), y = .fitted)) + theme_bw() + geom_point(colour = "black", size = 0.8)

gg + facet_grid(~ mag2)

