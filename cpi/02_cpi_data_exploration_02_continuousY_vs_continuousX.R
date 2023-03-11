# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\cpi")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR
#   - CPI (Consumer Price Index), Exchange Rage
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ----------
dat <- dat %>% mutate(yr_flg = ifelse(year >= 1979, 1, 0))




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(dat, aes(exr, cpi)) + geom_point(size = 2, alpha = 0.5) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ----------

gg + facet_grid(~ yr_flg)


