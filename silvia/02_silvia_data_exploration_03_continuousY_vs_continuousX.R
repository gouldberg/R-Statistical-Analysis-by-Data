# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\silvia")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  silvia
# ------------------------------------------------------------------------------

dat <- read.csv("silvia_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$TM <- as.factor(dat$TM)


dat$DEAL <- as.factor(dat$DEAL)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# P ~ KM
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(KM, P)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ----------

gg + facet_grid(~ TM)

gg + facet_grid(~ DEAL)

gg + facet_grid(~ TM + DEAL)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# Price down ~ KM
# ------------------------------------------------------------------------------


dat <- dat %>% mutate(pd = P - NP)



library(ggplot2)

gg <- ggplot(dat, aes(KM, pd)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ----------

gg + facet_grid(~ TM)

gg + facet_grid(~ DEAL)

gg + facet_grid(~ TM + DEAL)
