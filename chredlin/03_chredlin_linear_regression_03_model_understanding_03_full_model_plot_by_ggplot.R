setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

fitp <- cbind(linmod$model, pred = predict(linmod))


head(fitp)


colnames(fitp) <- c("involact", "race", "fire", "theft", "age", "logincome", "pred")

  

# ----------
library(ggplot2)

graphics.off()

gg1 <- ggplot(fitp, aes(x = race, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1






# ----------
gg2 <- ggplot(fitp, aes(x = logincome, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2


