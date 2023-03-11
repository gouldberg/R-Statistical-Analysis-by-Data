setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)


TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Stacking table:  3-way table into 2-way stacked table
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)


levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))


TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)


structable(Day ~ Network + Time, TV3)



# ----------
# interactive coding

TV3S <- as.matrix(structable(Day ~ Network + Time, TV3), sep = ":")



# ------------------------------------------------------------------------------
# Correspondence analysis 2D solution for stacked table, compared with 2-way table solution
# ------------------------------------------------------------------------------

library(FactoMineR)


par(mfrow = c(1,2))

res.ca2 <- CA(TV2)
res.ca3 <- CA(TV3S)



# ----------
par(mfrow = c(2,2))

plot(res.ca2, invisible = "row")
plot(res.ca2, invisible = "col")

plot(res.ca3, invisible = "col")
plot(res.ca3, invisible = "row")




# ----------
summary(res.ca2)

summary(res.ca3)



# -->
# 3-way table (stacked in 2-way) has dimensions up to 4.
# NBC, in total, contribution has 56%, which is slightly smaller than 59% in 2-way table solution









