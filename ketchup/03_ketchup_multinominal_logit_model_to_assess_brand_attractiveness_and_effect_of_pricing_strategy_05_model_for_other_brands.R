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
# data exploration:  price range
# ------------------------------------------------------------------------------


# the price for each brand, when selected or other brands selected

data <- data %>% mutate(
  choice.heinz = ifelse(Ketchup.choice == "heinz", 1, 0),
  choice.hunts = ifelse(Ketchup.choice == "hunts", 1, 0),
  choice.delmonte = ifelse(Ketchup.choice == "delmonte", 1, 0),
  choice.stb = ifelse(Ketchup.choice == "stb", 1, 0))


par(mfrow = c(2,2))
car::densityPlot(x = data$price.heinz, g = as.factor(data$choice.heinz), lty = c(2,1), col = c(gray(0.7), "black"), show.bw = TRUE, legend = FALSE)

car::densityPlot(x = data$price.hunts, g = as.factor(data$choice.hunts), lty = c(2,1), col = c(gray(0.7), "black"), show.bw = TRUE, legend = FALSE)

car::densityPlot(x = data$price.delmonte, g = as.factor(data$choice.delmonte), lty = c(2,1), col = c(gray(0.7), "black"), show.bw = TRUE, legend = FALSE)

car::densityPlot(x = data$price.stb, g = as.factor(data$choice.stb), lty = c(2,1), col = c(gray(0.7), "black"), show.bw = TRUE, legend = FALSE)




# ------------------------------------------------------------------------------
# Try 3 brands
# ------------------------------------------------------------------------------


library(mlogit)


data4 <- data %>% filter(Ketchup.choice %in% c("heintz", "stb", "hunts"))


data4[,"Ketchup.choice"] <- droplevels(data4[,"Ketchup.choice"])



# mlogit format:  buy history * choice
data_m4 <- mlogit.data(data4, varying=c(4,5,7), sep=".", shape="wide", choice="Ketchup.choice")


head(data_m4, 20)




# ----------
result4 <- mlogit(Ketchup.choice ~ price, data=data_m4)


summary(result4)





# ------------------------------------------------------------------------------
# Try stb against heintz,  stab against huntz
# ------------------------------------------------------------------------------


data4 <- data %>% filter(Ketchup.choice %in% c("heintz", "stb"))
# data4 <- data %>% filter(Ketchup.choice %in% c("hunts", "stb"))


data4[,"Ketchup.choice"] <- droplevels(data4[,"Ketchup.choice"])



# mlogit format:  buy history * choice
data_m4 <- mlogit.data(data4, varying=c(4,7), sep=".", shape="wide", choice="Ketchup.choice")
# data_m4 <- mlogit.data(data4, varying=c(5,7), sep=".", shape="wide", choice="Ketchup.choice")


head(data_m4, 20)




# ----------
result4 <- mlogit(Ketchup.choice ~ price, data=data_m4)


summary(result4)




# -->
# stb does not have unique attractiveness against heintz
# stb does have unique attractiveness against huntz

