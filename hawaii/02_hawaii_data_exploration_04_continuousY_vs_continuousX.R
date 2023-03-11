# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

Hawaii2 <- na.exclude(Hawaii)

plot(Rainfall ~ Year, data = Hawaii2, ylab = "Rainfall", xlab = "Year", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Hawaii2$Year, Hawaii2$Rainfall), col = "blue", lwd = 1)


plot(sqrt(Moorhen.Kauai) ~ Year, data = Hawaii2, ylab = "sqrt(Moorhen.Kauai)", xlab = "Year", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Hawaii2$Year, sqrt(Hawaii2$Moorhen.Kauai)), col = "blue", lwd = 1)


plot(sqrt(Moorhen.Kauai) ~ Rainfall, data = Hawaii2, ylab = "sqrt(Moorhen.Kauai)", xlab = "Rainfall", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Hawaii2$Rainfall, sqrt(Hawaii2$Moorhen.Kauai)), col = "blue", lwd = 1)





# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)


ggplot(Hawaii, aes(x = Year, y = Rainfall)) + xlab("Year") + ylab("Rainfall") + geom_point(alpha = 0.3) + stat_smooth()


ggplot(Hawaii, aes(x = Year, y = Moorhen.Kauai)) + xlab("Year") + ylab("Moorhen.Kauai") + geom_point(alpha = 0.3) + stat_smooth()


ggplot(Hawaii, aes(x = Rainfall, y = Moorhen.Kauai)) + xlab("Rainfall") + ylab("Moorhen.Kauai") + geom_point(alpha = 0.3) + stat_smooth()




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by scatterplot
# ------------------------------------------------------------------------------

formula <- Moorhen.Kauai ~ Rainfall

car::scatterplot(formula, data = Hawaii)



formula <- Moorhen.Kauai ~ Year

car::scatterplot(formula, data = Hawaii)
