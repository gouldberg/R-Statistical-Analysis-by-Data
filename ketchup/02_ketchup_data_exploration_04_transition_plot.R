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




# ------------------------------------------------------------------------------
# data exploration:  transition plot
#   - transition from first buy to second buy
#   - which brand has unique attractiveness ?
#
#   - Heinz share is largest and first buy to second buy is large, but taken over some share by Hunts
#     Comparatively, Huntz unique attractiveness is larger than Heinz and price strategy might be effective for Huntz
# ------------------------------------------------------------------------------

tmp <- data %>% mutate(Ketchup.id = Ketchup.id - 1)

data2 <- left_join(data %>% dplyr::select(Ketchup.hid, Ketchup.id, Ketchup.choice), tmp, by=c("Ketchup.hid", "Ketchup.id"))

data2 <- data2 %>% dplyr::select(choice1 = Ketchup.choice.x, choice2 = Ketchup.choice.y) %>% as.data.frame()

trans.mat <- table(data2)


head(trans.mat)



# ----------

library(Gmisc)

library(grid)


graphics.off()

cols <- c("forestgreen", "orange2", "cornflowerblue", "royalblue")

transitionPlot(trans.mat, fill_start_box=cols, fill_end_box=cols, 
               overlap_add_width=1.2, type_of_arrow="gradient", cex=1.5, min_lwd=unit(0.1,"mm"), max_lwd=unit(6,"mm"))


