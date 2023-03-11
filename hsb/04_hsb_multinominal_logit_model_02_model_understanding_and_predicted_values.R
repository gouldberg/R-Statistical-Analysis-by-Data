setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# Model performance:  Accuracy
# ------------------------------------------------------------------------------

xtabs(~ predict(mmodi) + hsb$prog)




# ------------------------------------------------------------------------------
# Predicted values:  varied by math
# ------------------------------------------------------------------------------

mathlevels <- 0:100

dat <- data.frame(math = mathlevels, ses = "middle", schtyp = "public", science = median(hsb$science), socst = median(hsb$socst))

( preds <- data.frame(math = mathlevels, predict(mmodi, dat, type = "probs")) )



# ----------
library(tidyr)

lpred <- gather(preds, prog, probability, -math)

ggplot(lpred, aes(x = math, y = probability, group = prog, linetype = prog)) + geom_line()




# ------------------------------------------------------------------------------
# Predicted values:  varied by ses and schtyp
# ------------------------------------------------------------------------------

dat <- expand.grid(ses = unique(hsb$ses), schtyp = unique(hsb$schtyp))

dat <- mutate(dat, math = median(hsb$math), science = median(hsb$science), socst = median(hsb$socst))


( preds <- data.frame(dat, round(predict(mmodi, dat, type = "probs"), 3)) )



# ----------
library(tidyr)

lpred <- gather(preds[,c("ses", "schtyp", "academic", "general", "vocation")], prog, probability, -ses, -schtyp)

ggplot(lpred, aes(x = ses, y = probability, group = prog, linetype = prog)) + geom_line() + facet_wrap(~ schtyp)


