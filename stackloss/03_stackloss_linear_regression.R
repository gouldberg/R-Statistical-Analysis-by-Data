# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# full variable
# ------------------------------------------------------------------------------


mod0 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = dat)


summary(mod0)



# ----------
car::Anova(mod0)




# ------------------------------------------------------------------------------
# excluding Acid.Conc.
# ------------------------------------------------------------------------------

mod1 <- update(mod0, . ~ . - Acid.Conc.)


summary(mod1)


car::Anova(mod1)



# ----------

anova(mod0, mod1)




# ----------
AIC(mod0, mod1)




