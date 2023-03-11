# setwd("//media//kswada//MyFiles//R//ketchup")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//ketchup")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Ketchup
# ------------------------------------------------------------------------------

data("Ketchup", package = "Ecdat")


dim(Ketchup)


str(Ketchup)


head(Ketchup)



# ----------
data <- Ketchup



# ------------------------------------------------------------------------------
# data exploration:  distribution of purchase ratio (for customer) by brand
# ------------------------------------------------------------------------------

library(reshape)


data <- data %>% mutate(cnt = 1)

tab2 <- as.data.frame(xtabs(cnt ~ Ketchup.hid + Ketchup.choice, data=data))

tab2 <- reshape::cast(tab2, formula=Ketchup.hid ~ Ketchup.choice, value="Freq")



# ----------
head(tab2)



# ----------
tab2 <- tab2 %>% mutate(
  sum = heinz + hunts + delmonte + stb,
  heinz.prop = heinz / sum,
  hunts.prop = hunts / sum,
  delmonte.prop = delmonte / sum,
  stb.prop = stb / sum)



# ----------
breaks <- seq(0.0, 1.0, by=0.1)

p1 <- ggplot(tab2, aes(x=heinz.prop)) + geom_histogram(breaks=breaks) + ylim(0,2000) + theme_minimal()

p2 <- ggplot(tab2, aes(x=hunts.prop)) + geom_histogram(breaks=breaks) + ylim(0,2000) + theme_minimal()

p3 <- ggplot(tab2, aes(x=delmonte.prop)) + geom_histogram(breaks=breaks) + ylim(0,2000) + theme_minimal()

p4 <- ggplot(tab2, aes(x=stb.prop)) + geom_histogram(breaks=breaks) + ylim(0,2000) + theme_minimal()

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)




# ------------------------------------------------------------------------------
# data exploration:  distribution of purchase ratio by brand  (stacked bar chart)
# ------------------------------------------------------------------------------

tab2 <- tab2 %>% mutate(
  heinz.hist = ifelse(heinz.prop > 0, 1, 0),
  hunts.hist = ifelse(hunts.prop > 0, 1, 0),
  delmonte.hist = ifelse(delmonte.prop > 0, 1, 0),
  stb.hist = ifelse(stb.prop > 0, 1, 0))


tab2[,"heinz.hist"] <- as.factor(tab2[,"heinz.hist"])

tab2[,"hunts.hist"] <- as.factor(tab2[,"hunts.hist"])

tab2[,"delmonte.hist"] <- as.factor(tab2[,"delmonte.hist"])

tab2[,"stb.hist"] <- as.factor(tab2[,"stb.hist"])




# ----------
# proportion by heinz.hist = 0 or 1
# Hunts customers (Hunts prop > 0)  -->  the ratio of Huntz buy is large

breaks <- seq(0.0, 1.0, by=0.1)

p1 <- ggplot(tab2, aes(x=heinz.prop, fill=heinz.hist, colour=heinz.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p2 <- ggplot(tab2, aes(x=hunts.prop, fill=heinz.hist, colour=heinz.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p3 <- ggplot(tab2, aes(x=delmonte.prop, fill=heinz.hist, colour=heinz.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p4 <- ggplot(tab2, aes(x=stb.prop, fill=heinz.hist, colour=heinz.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)




# ----------
# proportion by huntz.hist = 0 or 1
# Heintz customers (Heintz prop > 0)  -->  the ratio of no-buy Hunts is large

breaks <- seq(0.0, 1.0, by=0.1)

p1 <- ggplot(tab2, aes(x=heinz.prop, fill=hunts.hist, colour=hunts.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p2 <- ggplot(tab2, aes(x=hunts.prop, fill=hunts.hist, colour=hunts.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p3 <- ggplot(tab2, aes(x=delmonte.prop, fill=hunts.hist, colour=hunts.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

p4 <- ggplot(tab2, aes(x=stb.prop, fill=hunts.hist, colour=hunts.hist)) + geom_histogram(alpha=0.8, breaks=breaks, position="stack") + ylim(0,2000) + theme_minimal()

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)



