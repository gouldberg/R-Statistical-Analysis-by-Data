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

library(tidyverse)



# the price by each brand, where each brand is bought

par(mfrow = c(1,1))

tmp <- data %>% dplyr::select(Ketchup.choice, price.heinz, price.hunts, price.delmonte, price.stb) %>% gather(., key = "Ketchup.choice", value = "price")


car::densityPlot(x = tmp$price, g = as.factor(tmp$Ketchup.choice), 
#                 legend = FALSE,
                 col = c("black", "blue", "red", "darkgray")
                 )



# -->
# stb is lowest price brand



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



# -->
# Heinz are purchased at lower price


