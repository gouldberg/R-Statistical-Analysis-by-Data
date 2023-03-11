setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ----------
covars <- CEAQ %>% dplyr::select(age, grade, gender)





# ------------------------------------------------------------------------------
# Tree-Based DIF detection
#   - Based on multiple external metric or categorical variables, the algorithm tries to find splits for which the item parameters differ.
#     If no split is found, there is no DIF.
# ------------------------------------------------------------------------------

library(psychotree)


# Note that item responses, in order to be processed by the raschtree function, have to be provided as object of class "itemresp"

df <- data.frame(res = itemresp(itceaq), covars)


head(df)


mscale(df$res)


graphics.off()
plot(df$res)




# ----------
# Rating Scale Model
set.seed(1)


rstr <- rstree(res ~ age + grade + gender, 
                 data = df, vcov = "info", minsize = 10, nrep = 1e5)



# ----------
plot(rstr)



# -->
# note that in Node 8, all item has small "1" value --> suggesting that test information for this node is high.



# ----------
# item parameters for each node
round(itempar(rstr), 2)


