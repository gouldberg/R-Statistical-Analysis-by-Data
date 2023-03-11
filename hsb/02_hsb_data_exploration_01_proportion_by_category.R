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
# data exploration:  proportion by category
# ------------------------------------------------------------------------------

summary(hsb)



# ----------
# Relationship between program and gender

hgp <- group_by(hsb, gender, prog) %>% dplyr::summarise(count = n()) %>% group_by(gender) %>% mutate(total = sum(count), proportion = count / total)


hgp


ggplot(hgp, aes(x = gender, y = proportion, group = prog, linetype = prog)) + geom_line()




# ----------
# Relationship between program and gender

hsb$ses <- factor(hsb$ses, levels = c("low", "middle", "high"))


sgp <- group_by(hsb, ses, prog) %>% dplyr::summarise(count = n()) %>% group_by(ses) %>% mutate(total = sum(count), proportion = count / total)


sgp


ggplot(sgp, aes(x = ses, y = proportion, group = prog, linetype = prog)) + geom_line()




# ----------
# Relationship between program and reading

rgp <- mutate(hsb, readgp = cut_number(read, 7)) %>% group_by(readgp, prog) %>% dplyr::summarise(count = n()) %>% group_by(readgp) %>% mutate(total = sum(count), proportion = count / total)


rgp


ggplot(rgp, aes(x = readgp, y = proportion, group = prog, linetype = prog)) + geom_line()




# ----------
# Relationship between program and math

mgp <- mutate(hsb, mathgp = cut_number(math, 7)) %>% group_by(mathgp, prog) %>% dplyr::summarise(count = n()) %>% group_by(mathgp) %>% mutate(total = sum(count), proportion = count / total)


mgp


ggplot(mgp, aes(x = mathgp, y = proportion, group = prog, linetype = prog)) + geom_line()
