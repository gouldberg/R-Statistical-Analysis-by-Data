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
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------


set.seed(123)


ltsmod <- ltsreg(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)


ltsmod


coef(ltsmod)

coef(mod_step)



# -->
# very different estimation ...

