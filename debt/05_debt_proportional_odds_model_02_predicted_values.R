setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)



# ------------------------------------------------------------------------------
# Model performance:  Accuracy
# ------------------------------------------------------------------------------

xtabs(~ predict(pomodi) + na.omit(debt)$ccarduse)




# ------------------------------------------------------------------------------
# Predicted values:  varied by prodebt
# ------------------------------------------------------------------------------

prodebtlevels <- seq(0, 5.5, by = 0.1)


dat <- data.frame(prodebt = prodebtlevels, incomegp = median(debt$incomegp, na.rm = T),
                  agegp = median(debt$agegp, na.rm = T),
                  bankacc = median(debt$bankacc, na.rm = T),
                  bsocacc = median(debt$bsocacc, na.rm = T),
                  cigbuy = median(debt$cigbuy, na.rm = T))


( preds <- data.frame(prodebt = prodebtlevels, predict(pomodi, dat, type = "probs")) )



# ----------
library(tidyr)

lpred <- gather(preds, ccarduse, probability, -prodebt)

ggplot(lpred, aes(x = prodebt, y = probability, group = ccarduse, linetype = ccarduse)) + geom_line()




# ------------------------------------------------------------------------------
# Predicted values:  varied by cigbuy
# ------------------------------------------------------------------------------

dat <- data.frame(prodebt = median(debt$prodebt, na.rm = T), incomegp = median(debt$incomegp, na.rm = T),
                  agegp = median(debt$agegp, na.rm = T),
                  bankacc = median(debt$bankacc, na.rm = T),
                  bsocacc = median(debt$bsocacc, na.rm = T),
                  cigbuy = unique(na.omit(debt)$cigbuy))


( preds <- data.frame(cigbuy = unique(na.omit(debt)$cigbuy), predict(pomodi, dat, type = "probs")) )



