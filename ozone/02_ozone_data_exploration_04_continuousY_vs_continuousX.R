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
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(ozone, aes(x = temp, y = O3)) + geom_point(size = 1) + geom_smooth()


ggplot(ozone, aes(x = ibh, y = O3)) + geom_point(size = 1) + geom_smooth() + theme(axis.text.x = element_text(angle = 90))


ggplot(ozone, aes(x = ibt, y = O3)) + geom_point(size = 1) + geom_smooth()



# -->
# we can see a somewhat nonlinear relationship in all three cases.

