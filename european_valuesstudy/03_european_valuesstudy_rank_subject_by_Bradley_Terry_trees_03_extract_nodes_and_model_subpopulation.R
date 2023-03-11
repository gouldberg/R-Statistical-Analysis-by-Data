# setwd("//media//kswada//MyFiles//R//european_valuesstudy")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//european_valuesstudy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  European Values Study
# ------------------------------------------------------------------------------

data("EuropeanValuesStudy", package = "psychotree")


str(EuropeanValuesStudy)


car::some(EuropeanValuesStudy)



evs <- na.omit(EuropeanValuesStudy)



# ------------------------------------------------------------------------------
# Extract node number
# ------------------------------------------------------------------------------


# extract node number
bt_node <- predict(bt, newdata = evs, type = c("node"))



# ----------
# node number 9 and 13
idx9 <- which(bt_node == 9)

idx13 <- which(bt_node == 13)


var_obj <- c("gender", "employment", "paircomp")

evs[idx9, var_obj]

evs[idx13, var_obj]




# ------------------------------------------------------------------------------
# Bradley Terry Trees for subpopulation
# ------------------------------------------------------------------------------


evs_node9 <- evs[idx9,]

evs_node13 <- evs[idx13,]



bt_node9 <- bttree(paircomp ~ gender + eduage + birthyear + marital + employment + income + country2,
             data = evs_node9, alpha = 0.3)



bt_node13 <- bttree(paircomp ~ gender + eduage + birthyear + marital + employment + income + country2,
                   data = evs_node13, alpha = 0.3)



# -----------
plot(bt_node9, abbreviate = 2)


plot(bt_node13, abbreviate = 2)




# ------------------------------------------------------------------------------
# Bradley Terry Trees:  test parameter instability at each node
# ------------------------------------------------------------------------------

library("strucchange")


sctest(bt_node9, node = 3)


sctest(bt_node13, node = 7)


