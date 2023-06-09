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



# ----------
# price down
dat <- dat %>% mutate(pd = P - NP)


# KM == 0
dat <- dat %>% mutate(km_z = KM == 0)

mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)




# ------------------------------------------------------------------------------
# models
# ------------------------------------------------------------------------------

mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)

mod1 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z, data = dat)

mod2 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z + KM : TM, data = dat)

mod3 <- lm(P ~ NP + KM + TM + Yr + km_z, data = dat)

mod4 <- lm(P ~ NP + KM + TM + I(Yr^2) + I((24 - CT)^2) + km_z, data = dat)

mod_step <- lm(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)





# ------------------------------------------------------------------------------
# M-estimation by Huber method
# ------------------------------------------------------------------------------

library(MASS)


# this produces singular message
# rlmod <- rlm(P ~ (NP + KM + TM + Yr + I(Yr^2) + CT + I(24 - CT) + DEAL + km_z + I(CT == 0))^2, data = dat)


rlmod <- rlm(P ~ (NP + KM + TM + Yr + CT + DEAL)^2, data = dat)


summary(rlmod)


summary(mod_step)



# -->
# Coefficient of KM is positive ... very different estimation



