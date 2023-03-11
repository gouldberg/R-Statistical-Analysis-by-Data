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
# data exploration:  continuousY vs. categoryX
# Price down ~ TM, DEAL
# ------------------------------------------------------------------------------


dat <- dat %>% mutate(pd = P - NP)



boxplot(pd ~ TM, data = dat, varwidth = TRUE)


boxplot(pd ~ TM + DEAL, data = dat, varwidth = TRUE)



# -->
# for manual car:  price decreasing is smaller
# for dealer car:  price decreasing is smaller




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. categoryX
# Price down ~ Yr
# ------------------------------------------------------------------------------

boxplot(pd ~ Yr, data = dat, varwidth = TRUE)

boxplot(pd ~ Yr + DEAL, data = dat, varwidth = TRUE)



