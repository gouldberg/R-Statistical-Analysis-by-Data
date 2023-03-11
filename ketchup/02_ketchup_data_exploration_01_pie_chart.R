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
# data exploration:  share by brand
# ------------------------------------------------------------------------------

( tab <- table(data$Ketchup.choice) )



# ----------
addmargins(tab)


round(addmargins(prop.table(tab)), digits = 3)



# ----------
pie(tab, label = paste0(sprintf("%2.1f", prop.table(tab)*100), "%"), clockwise=TRUE, main="brand share", col = rainbow(nrow(tab)), border=FALSE)


legend("topright", c("heinz", "hunts", "delmonte", "stb"), cex = 1.0,  fill = rainbow(nrow(tab)))



# -->
# heinz --> stb --> hunts --> delmonte


