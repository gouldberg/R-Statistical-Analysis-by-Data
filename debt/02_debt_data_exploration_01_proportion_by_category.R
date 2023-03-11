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
# data exploration:  proportion by category
# ------------------------------------------------------------------------------

summary(debt)



# -->
# There are some NA values


# ----------
# Relationship between ccarduse and incomegp

igp <- group_by(debt, incomegp, ccarduse) %>% dplyr::summarise(count = n()) %>% group_by(incomegp) %>% mutate(total = sum(count), proportion = count / total)


igp


ggplot(igp, aes(x = incomegp, y = proportion, group = ccarduse, linetype = ccarduse)) + geom_line()




# ----------
# Relationship between ccarduse and prodebt

debt$ccarduse <- factor(debt$ccarduse, levels = c(1, 2, 3), ordered = TRUE)


pgp <- mutate(debt, prodebtgp = cut_number(prodebt, 7)) %>% group_by(prodebtgp, ccarduse) %>% dplyr::summarise(count = n()) %>% group_by(prodebtgp) %>% mutate(total = sum(count), proportion = count / total)


pgp


ggplot(pgp, aes(x = prodebtgp, y = proportion, group = ccarduse, linetype = ccarduse)) + geom_line()
