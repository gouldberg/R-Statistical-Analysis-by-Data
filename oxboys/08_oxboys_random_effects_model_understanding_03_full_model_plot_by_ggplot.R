setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ----------
library(nlme)

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Subject = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

ox.fitp <- cbind(Oxboys, height_pred = predict(mod_obj, type = "response"))

# ox.fitp <- fortify(mod_obj)

head(ox.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(ox.fitp, aes(x = age, y = height_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_wrap(~ Subject) + stat_smooth()
