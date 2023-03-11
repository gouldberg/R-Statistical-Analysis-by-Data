setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ----------
# we here remove duplicated rows (but "length" is changed from 6 to 7 within year) for Science Digest year 45
# also included == 1:  year 53 to 79 for palane balancing

MagazinePrices2 <- MagazinePrices %>% filter(! (year == 45 & magazine == "Science Digest" & length == 7)) %>% filter(included == 1)




# ------------------------------------------------------------------------------
# fit logit models by pglm
# ------------------------------------------------------------------------------

library(pglm)


m.pl <- pglm(change ~ length + cuminf + cumsales,
               data = MagazinePrices2,
               family = binomial(link = "logit"))


summary(m.pl)



# ----------
car::compareCoefs(logitS, logitD, m.pl)



# -->
# Now the length effect is NOT significant...


