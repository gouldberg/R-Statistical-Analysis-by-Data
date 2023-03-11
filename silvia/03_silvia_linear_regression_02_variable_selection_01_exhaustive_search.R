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
# Exhaustively searches all possible combinations of the predictors by regsubsets
# ------------------------------------------------------------------------------


# check model for all 2-way interactions
mod_int2 <- lm(P ~ (NP + KM + TM + Yr + CT + DEAL + km_z)^2, data = dat)


summary(mod_int2)




# ----------
library(leaps)



# for each size of model p, it finds the variables that produce the minimum RSS
b <- regsubsets(P ~ (NP + KM + TM + Yr + I(Yr^2) + CT + I(24 - CT) + DEAL + km_z)^2, data = dat, method = "exhaustive", nvmax = 12, really.big = TRUE)


b



# ----------
( rs <- summary(b) )


rs$which



# ----------
( AIC <- nrow(dat) * log(rs$rss / nrow(dat)) + (1:13) * 2 )


plot(AIC ~ I(1:13), ylab = "AIC", xlab = "Number of Predictors")




# ----------
rs$which[13,][which(rs$which[13,] == "TRUE")]






