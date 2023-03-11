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
# data exploration:  continuousY vs. continousX
# stack.loss ~ Air.Flow
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(Air.Flow, stack.loss)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# stack.loss ~ Water.Temp
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(Water.Temp, stack.loss)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# taste ~ Lactic
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(Acid.Conc., stack.loss)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg


