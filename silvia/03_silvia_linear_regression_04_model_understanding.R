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




# ----------
mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)

mod1 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z, data = dat)

mod2 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z + KM : TM, data = dat)

mod3 <- lm(P ~ NP + KM + TM + Yr + km_z, data = dat)

mod4 <- lm(P ~ NP + KM + TM + I(Yr^2) + I((24 - CT)^2) + km_z, data = dat)

mod_step <- lm(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)


mod_obj <- mod3

mod_obj <- mod_step



# ------------------------------------------------------------------------------
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


eff <- effects::allEffects(mod_obj)


eff



# ----------
# plot main effets of each variable

plot(eff)

# plot(predictorEffects(mod_obj))

# predictorEffects(mod_obj)




# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------


# effect plots for several predictors jointly or full-model plots

plot(Effect(c("KM", "TM"), mod_obj), 
     confint = list(style = "bands"),
#     confint = list(style = "bars"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))




# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

fitp <- cbind(mod_obj$model, pred = predict(mod_obj))


head(fitp)



library(ggplot2)

graphics.off()

gg1 <- ggplot(fitp, aes(x = KM, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1


gg1 + facet_grid(~ TM)




# ----------
gg2 <- ggplot(fitp, aes(x = Yr, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2


gg2 + facet_grid(~ TM)

