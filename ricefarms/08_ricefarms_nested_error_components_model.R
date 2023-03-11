setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# We use id and region as group
# ------------------------------------------------------------------------------

# R1, R2, R3 gives same indexing

R1 <- pdata.frame(RiceFarms, index = c(id = "id", time = NULL, group = "region"))

# R2 <- pdata.frame(RiceFarms, index = c(id = "id", group = "region"))
# R3 <- pdata.frame(RiceFarms, index = c(id = "id", group = "region"))


head(index(R1))



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)


# Swamy and Arora model
nswar <- plm(rice.eq, data = R1, model = "random", effect = "nested", random.method = "swar", index = c(group = "region"))


summary(nswar)



# ----------
# Try other estimators

namem <- update(nswar, random.method = "amemiya")

nwalhus <- update(nswar, random.method = "walhus")

iswar <- update(nswar, effect = "individual")

iwith <- update(nswar, model = "within", effect = "individual")



# ----------
# convert regression output
library(texreg)


screenreg(list("fe-id" = iwith, "re-id" = iswar, "Swamy_Arora" = nswar, "Wallas-Hussein" = nwalhus, "Amemiya" = namem), digits = 3)


