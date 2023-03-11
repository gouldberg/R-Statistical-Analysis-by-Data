setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
summary(qlmod)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

mam.fitp <- cbind(qlmod$model, pdr_pred = predict(qlmod, type = "response"))


head(mam.fitp)

colnames(mam.fitp) <- c("pdr", "log_b", "log_l", "danger", "pdr_pred")



# ----------
library(ggplot2)

graphics.off()

gg <- ggplot(mam.fitp, aes(x = log_b, y = pdr_pred)) + theme_bw() + geom_line(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg
