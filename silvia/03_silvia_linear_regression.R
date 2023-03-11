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




# ------------------------------------------------------------------------------
# simple linear regression
# ------------------------------------------------------------------------------


# no km_z
mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)


summary(mod0)



# ----------
# with km_z
mod1 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z, data = dat)


summary(mod1)





# ------------------------------------------------------------------------------
# KM * TM interaction
# ------------------------------------------------------------------------------


mod2 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z + KM : TM, data = dat)


summary(mod2)




# -->
# KM : TM is not significant




# ------------------------------------------------------------------------------
# without CT and DEAL
# ------------------------------------------------------------------------------


mod3 <- lm(P ~ NP + KM + TM + Yr + km_z, data = dat)


summary(mod3)




# not much difference
anova(mod3, mod2)




# ------------------------------------------------------------------------------
# with max(CT) - CT and Yr^2
# ------------------------------------------------------------------------------


mod4 <- lm(P ~ NP + KM + TM + I(Yr^2) + I((24 - CT)^2) + km_z, data = dat)


summary(mod4)



# cannot compare
anova(mod3, mod4)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------


AIC(mod0, mod1, mod2, mod3, mod4)



# -->
# mod 3 is best but close to mod1 (mod1 is not significant model)




