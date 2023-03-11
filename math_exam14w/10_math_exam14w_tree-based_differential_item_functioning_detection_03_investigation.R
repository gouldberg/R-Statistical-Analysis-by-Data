setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathExam14W
# ------------------------------------------------------------------------------

data("MathExam14W", package = "psychotools")


str(MathExam14W)

dim(MathExam14W)


head(MathExam14W)



# ----------
itmath <- as.list.data.frame(MathExam14W$solved)

covars <- MathExam14W[,3:9]

car::some(covars)



# ----------
library(psychotree)

mex <- data.frame(solved = itemresp(itmath), covars)
mex <- subset(mex, nsolved > 0 & nsolved < 13)

mex$tests <- ordered(mex$tests)
mex$nsolved <- ordered(mex$nsolved)
mex$attempt <- ordered(mex$attempt)




# ------------------------------------------------------------------------------
# Extract node, item information, and test information
# ------------------------------------------------------------------------------

pred_node <- predict(mrt, ndwdata = mex, type = c("node"))

item_info <- predict(mrt, ndwdata = mex, type = c("item-information"))

test_info <- predict(mrt, ndwdata = mex, type = c("test-information"))



tmp_dat <- data.frame(rownames(mex), pred_node, item_info, test_info)

colnames(tmp_dat)[1] <- "id"

tmp_dat$pred_node <- as.factor(tmp_dat$pred_node)

str(tmp_dat)



# ------------------------------------------------------------------------------
# test information by node
# ------------------------------------------------------------------------------


boxplot(test_info ~ pred_node, data = tmp_dat, varwidth = TRUE, 
        xlab = "node", ylab = "test information", las = 1)



# ------------------------------------------------------------------------------
# item information by node
# ------------------------------------------------------------------------------

library(tidyr)


tmp_dat2 <- gather(tmp_dat, key = "item", value = "iteminfo", -id, -pred_node) %>% dplyr::filter(item != "test_info")


tmp_dat2$item <- abbreviate(tmp_dat2$item)


head(tmp_dat2)


str(tmp_dat2)



# ----------
bwplot(iteminfo ~ pred_node | item, data = tmp_dat2)



# -->
# for example:
# node 7:  very high itemparameter in "solvedpayflow" and very low in "solvedhesse"
# Here in item information, node 7's "solvedpayflow" is lower than other nodes,
# and "solvedhesse" is lower than othernodes

# if person is classified as node 7, it is difficult to discriminate their ability by those items




bwplot(iteminfo ~ item | pred_node, data = tmp_dat2)
# bwplot(iteminfo ~ item | pred_node, data = tmp_dat2 %>% dplyr::filter(pred_node == 7))



# -->
# item parameter of "solvedintegral" for node 7 = 0.31 (close to zero)
# --> item information of "solvedintegral" for node 7 is highest




# ---------
plot(mrt)

round(itempar(mrt), 2)

