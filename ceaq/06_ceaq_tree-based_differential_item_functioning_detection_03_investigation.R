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
# Extract node, item information, and test information
# ------------------------------------------------------------------------------

pred_node <- predict(rstr, ndwdata = df, type = c("node")) %>% as.data.frame()
pred_node$id <- row.names(pred_node)
colnames(pred_node) <- c("pred_node", "id")


item_info <- predict(rstr, ndwdata = df, type = c("item-information")) %>% as.data.frame()
item_info$id <- row.names(item_info)


test_info <- predict(rstr, ndwdata = df, type = c("test-information")) %>% as.data.frame()
test_info$id <- row.names(test_info)
colnames(test_info) <- c("test_info", "id")



tmp_dat <- data.frame(rownames(df))

colnames(tmp_dat)[1] <- "id"


tmp_dat <- tmp_dat %>% left_join(., pred_node, by = "id") %>% left_join(., item_info, by = "id") %>% left_join(., test_info, by = "id")

tmp_dat$pred_node <- as.factor(tmp_dat$pred_node)

str(tmp_dat)



# ------------------------------------------------------------------------------
# test information by node
# ------------------------------------------------------------------------------


boxplot(test_info ~ pred_node, data = tmp_dat, varwidth = TRUE, 
        xlab = "node", ylab = "test information", las = 1)



plot(rstr)



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


bwplot(iteminfo ~ item | pred_node, data = tmp_dat2)


bwplot(iteminfo ~ item | pred_node, data = tmp_dat2 %>% dplyr::filter(pred_node == 8))


round(itempar(rstr), 2)

  

# -->
# for Node 8:
# item 2, 4, 5, 7, 12, 15, 16 has more than 0.5 in item information
# item 13 is almost 0.3 (low, but relatively higher than other nodes)




# ------------------------------------------------------------------------------
#  ceaq2:  I'm happy when the teacher says my friend did a good job.
#  ceaq4:  I understand how other kids feel.
#  ceaq5:  I would feel bad if my mom's friend got sick.
#  ceaq7:  I feel happy when my friend gets a good grade.
#  ceaq12:  I feel sorry for kids who can't find anyone to hang out with.
#  * ceaq15:  It would bother me if my friend got grounded.
#  ceaq16:  When I see someone who is happy, I feel happy too.
#
#  ** ceaq13:  Seeing a kid who is crying makes me feel like crying.
#
#  *:  removed lated due to misfit by rating scale model
#  **:  threshold by rating scale model is very much different from other items
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Person Item Map
# ------------------------------------------------------------------------------

plotPImap(fitrsm2, latdim = "Empathy", main = "Person-Item Map CEAQ")


# -->
# solid dots:  the item location parameters
# hollow dots:  the thretholds parameters


