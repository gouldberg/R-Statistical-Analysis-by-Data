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
# Stepwise selection
# ------------------------------------------------------------------------------

# check model for all 2-way interactions
mod_int2 <- lm(P ~ (NP + KM + TM + Yr + I(Yr^2) + CT + I(24 - CT) + DEAL + km_z + I(CT == 0))^2, data = dat)


summary(mod_int2)




# ---------
mod_step <- step(mod_int2)


summary(mod_step)





# ----------
mod_step <- lm(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)


summary(mod_step)

