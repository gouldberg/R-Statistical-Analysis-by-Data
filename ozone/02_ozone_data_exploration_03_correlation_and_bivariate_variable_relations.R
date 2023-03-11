setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

# data(ozone, package="faraway")

ozone <- read.csv(file = "ozone.txt", header = T, sep = "\t")


str(ozone)




# ------------------------------------------------------------------------------
# Data exploration:  pairs plot
# ------------------------------------------------------------------------------


MyVar <- c("vh", "wind", "humidity", "temp", "ibh", "dpg", "ibt", "vis", "O3")


psych::pairs.panels(ozone[,MyVar], stars = TRUE)


psych::pairs.panels(ozone[,MyVar], method = "spearman", stars = TRUE)





# ------------------------------------------------------------------------------
# Data exploration:  corrplot
# ------------------------------------------------------------------------------

library(corrplot)


cor_mat <- cor(ozone, method = "spearman")


corrplot(cor_mat, hclust.method = "ward.D2", addrect = TRUE)




# ------------------------------------------------------------------------------
# Data exploration:  paris plot by scatterplot matrix
# ------------------------------------------------------------------------------


library(car)


formula <- ~ temp + ibh + ibt + O3


scatterplotMatrix(formula, data = ozone,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




