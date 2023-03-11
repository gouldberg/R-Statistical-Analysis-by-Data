
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

# https://rdrr.io/cran/nlmeU/src/inst/scriptsR2.15.0/Ch12.R


# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# model list
# ------------------------------------------------------------------------------

library(nlme)



lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f : time.f)

lm6.1 <- lm(lm1.form, data = armd)



# ----------
fm9.1 <- gls(lm1.form, weights = varIdent(form = ~1 | time.f), data = armd)

fm9.2 <- update(fm9.1, weights = varPower(form = ~ time))
fm9.3 <- update(fm9.1, weights = varPower(form = ~ time | treat.f))
fm9.4 <- update(fm9.1, weights = varPower())
fm9.5 <- update(fm9.1, weights = varPower(fixed = 1))



# ----------
Vg1 <- nlme::Variogram(fm9.2, form = ~ time | subject)
# Vg1 <- nlme::Variogram(fm9.2, form = ~ time | subject, robust = TRUE)



# ----------
fm12.1 <- gls(lm1.form, weights = varPower(form = ~time), 
              correlation = corCompSymm(form = ~1 | subject),
              data = armd)

fm12.2 <- update(fm9.2, correlation = corAR1(form = ~ tp | subject), data = armd)

fm12.3 <- update(fm12.2, correlation = corSymm(form = ~ tp | subject), data = armd)

fmA.vc <- update(fm12.3, weights = varIdent(form = ~1 | time.f))




# ----------
lm1a.form <- formula (visual ~ visual0 + time.f + treat.f + time.f:treat.f)   
fm12.3a <- update(fm12.3, lm1a.form, method = "ML", data = armd)



# linear effect of continuous TIME variable

lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)
fm12.4 <- update(fm12.3, lm2.form, method = "ML", data = armd)



# removing interactions

lm3.form <-  update(lm2.form, . ~ . - treat.f:time)
fm12.5 <- update(fm12.3, lm3.form, method = "ML", data = armd)




lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f : time)



# ------------------------------------------------------------------------------
# random intercept and slopes
# ------------------------------------------------------------------------------

fm16.1 <- lme(lm2.form, random = ~ 1 | subject, data = armd)

fm16.2 <- update(fm16.1, weights = varPower(form = ~ time), data = armd)

fm16.3 <- update(fm16.2, random = ~ 1 + time | subject, data = armd)

fm16.4 <- update(fm16.3, random = list(subject = pdDiag(~time)), data = armd)

fm16.5 <- update(fm16.4, lm3.form, data = armd)


fm16.6 <- update(fm16.3, weights = varIdent(form = ~ 1 | time.f))
