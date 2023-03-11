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
# Hunts have unique attractiveness compared to Heintz ?   (intercept is significant ??)
# ------------------------------------------------------------------------------

library(mlogit)


data3 <- data %>% filter(Ketchup.choice %in% c("heinz", "hunts"))



# brands are only Heintz and Huntz
data3[,"Ketchup.choice"] <- droplevels(data3[,"Ketchup.choice"])



# mlogit format:  buy history * choice
data_m <- mlogit.data(data3, varying=4:5, sep=".", shape="wide", choice="Ketchup.choice")


head(data_m, 20)



# -->
# 1st record:  1st customer and 1st buy history  ->  heinz is selected
# 2nd record:  1st customer and 1st buy history  ->  huntz is NOT selected
# (one record into 2 records (TRUE and FALSE in Ketchup.choice))




# ----------
result <- mlogit(Ketchup.choice ~ price, data=data_m)



summary(result)



# -->
# Hunts intercept and coefficient for price is significant
# Hunts price down will increase Huntz buy




# ------------------------------------------------------------------------------
# model
# ------------------------------------------------------------------------------


# hunts intercept
coef1 <- result$coefficients[1]


# price coef
coef2 <- result$coefficients[2]



# ----------
# price range
x1 <- seq(0.1, 1.8, length=18)

x2 <- x1


d <- expand.grid(x1=x1, x2=x2)

d$p1 <- exp(coef2*d$x1)/(exp(coef2*d$x1)+exp(coef1+coef2*d$x2))

d$p2 <- exp(coef1+coef2*d$x2)/(exp(coef2*d$x1)+exp(coef1+coef2*d$x2))



# ----------
d %>% arrange(desc(x2), desc(x1)) %>% head(20)



# -->
# Huntz decrease price (x1) and will get share (p1)


