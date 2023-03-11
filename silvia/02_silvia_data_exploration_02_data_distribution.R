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
# data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(dat)




# ----------
psych::describe(dat)




# ----------
car::densityPlot(dat$P)

table(dat$NP)



# -->
# almost 2 modality data



# ----------
car::densityPlot(dat$KM)



# ----------
barplot(table(dat$Yr))



# ----------
barplot(table(dat$CT))




# ----------
mosaicplot(xtabs(~ TM + DEAL, data = dat))



# ----------
xtabs(~ NP + TM, data = dat)
