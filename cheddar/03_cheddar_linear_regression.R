# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# full variable
# ------------------------------------------------------------------------------


mod0 <- lm(taste ~ Acetic + H2S + Lactic, data = dat)


summary(mod0)



# ----------
car::Anova(mod0)




# ------------------------------------------------------------------------------
# excluding Acetic
# ------------------------------------------------------------------------------

mod1 <- update(mod0, . ~ . - Acetic)


summary(mod1)


car::Anova(mod1)



# ----------

anova(mod0, mod1)




# ----------
AIC(mod0, mod1)




