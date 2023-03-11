# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)





# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


plot(predictorEffects(mod3))




# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

fitp <- cbind(dat, pred = predict(mod3))


head(fitp)



library(ggplot2)

graphics.off()

gg1 <- ggplot(fitp, aes(x = Air.Flow, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1




# ----------
gg2 <- ggplot(fitp, aes(x = Water.Temp, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2

