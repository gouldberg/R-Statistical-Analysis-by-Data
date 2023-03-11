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

xtabs(~ predict(mmodi) + na.omit(debt)$ccarduse)


xtabs(~ predict(pomodi) + na.omit(debt)$ccarduse)


xtabs(~ predict(phmodi) + na.omit(debt)$ccarduse)


# -->
# not good at for "general"




# ------------------------------------------------------------------------------
# Predicted values:  varied by cigbuy
# ------------------------------------------------------------------------------

dat <- data.frame(prodebt = median(debt$prodebt, na.rm = T), incomegp = median(debt$incomegp, na.rm = T),
                  agegp = median(debt$agegp, na.rm = T),
                  bankacc = median(debt$bankacc, na.rm = T),
                  bsocacc = median(debt$bsocacc, na.rm = T),
                  house = median(debt$house, na.rm = T),
                  locintrn = median(debt$locintrn, na.rm = T),
                  cigbuy = unique(na.omit(debt)$cigbuy))


( preds_m <- data.frame(cigbuy = unique(na.omit(debt)$cigbuy), predict(mmodi, dat, type = "probs")) )

( preds_po <- data.frame(cigbuy = unique(na.omit(debt)$cigbuy), predict(pomodi, dat, type = "probs")) )

( preds_ph <- data.frame(cigbuy = unique(na.omit(debt)$cigbuy), predict(phmodi, dat, type = "probs")) )



# -->
# cigby = 1 and ccarduse = 2 (occasionally) is highest in proportional hazards model


