# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# taste ~ Acetic
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(Acetic, taste)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# taste ~ Acetic
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(H2S, taste)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# taste ~ Lactic
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(Lactic, taste)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg


