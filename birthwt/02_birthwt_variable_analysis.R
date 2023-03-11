setwd("//media//kswada//MyFiles//R//birthwt")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birthwt
#   - Data on 189 babies born at Baystate Medical Center, Springfield, MA during 1986.
#   - The quantitative response is bwt (birth weight in grams), and this is also recoded as low, a binary variable corresponding to bwt < 2500 (2.5 kg).
# ------------------------------------------------------------------------------
data("birthwt", package = "MASS")

data <- birthwt

dim(data)
str(data)


# ----------
Hmisc::describe(data)



# ----------
data$race <- factor(data$race, labels = c("white", "black", "other"))
data$ptd <- factor(data$ptl > 0)  # premature labors
data$ftv <- factor(data$ftv)  # physician visits
levels(data$ftv)[-(1:2)] <- "2+"
data$smoke <- factor(data$smoke > 0)
data$ht <- factor(data$ht > 0)
data$ui <- factor(data$ui > 0)

data$low_fac <- factor(data$low, levels = c(0, 1))



# ------------------------------------------------------------------------------
# Overall:  Margianl plots by gpairs() showing all bivariate margianl relations
# ------------------------------------------------------------------------------
library(gpairs)

gpairs(data[, c("low_fac", "age", "lwt", "race", "smoke", "ht", "ui", "ftv", "bwt", "ptd")],
       diag.pars = list(fontsize = 16, hist.color = "lightgray"),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))



# ------------------------------------------------------------------------------
# Relationship: contibuout X (bwt) vs. continuous X (age, lwt)
# ------------------------------------------------------------------------------
library(caret)

featurePlot(data[,c("age","lwt")], data[,"bwt"],
#            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))



# ------------------------------------------------------------------------------
# Relationship: binary Y (low) vs. continuous X (age, lwt)
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
with(data, logi.hist.plot(age, low, type = "hist",
                          counts = TRUE, ylabel = "Probability", xlab = "age", col.hist = "lightblue"))

par(mfrow=c(1,1))
with(data, logi.hist.plot(lwt, low, type = "hist",
                          counts = TRUE, ylabel = "Probability", xlab = "lwt", col.hist = "lightblue"))



# ------------------------------------------------------------------------------
# Relationship: binary Y (low) vs. categorical X (..)
# ------------------------------------------------------------------------------
gpairs(data[, c("low_fac", "race", "smoke", "ptl", "ht", "ui", "ftv", "ptd")],
       diag.pars = list(fontsize = 16, hist.color = "lightgray"),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))


plot(low_fac ~ race, data = data, col = c("blue", "orange"))  # --> black
plot(low_fac ~ smoke, data = data, col = c("blue", "orange"))  # --> TRUE
plot(low_fac ~ ht, data = data, col = c("blue", "orange")) # --> TRUE
plot(low_fac ~ ui, data = data, col = c("blue", "orange")) # --> TRUE
plot(low_fac ~ ftv, data = data, col = c("blue", "orange"))
plot(low_fac ~ ptd, data = data, col = c("blue", "orange")) # --> TRUE



# ------------------------------------------------------------------------------
# Conditional plot:  binary Y (low) vs. continuous X (age) by categorical (..)
# ------------------------------------------------------------------------------
# by race --> black
gg <- ggplot(data, aes(age, low, color = race)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = race))



# ----------
# by smoke
gg <- ggplot(data, aes(age, low, color = smoke)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = smoke))



# ----------
# by ht --> TRUE
gg <- ggplot(data, aes(age, low, color = ht)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ht))



# ----------
# by ui
gg <- ggplot(data, aes(age, low, color = ui)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ui))



# ----------
# by ptd
gg <- ggplot(data, aes(age, low, color = ptd)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ptd))



# ------------------------------------------------------------------------------
# Conditional plot:  binary Y (low) vs. continuous X (lwt) by categorical (..)
# ------------------------------------------------------------------------------
# by race
gg <- ggplot(data, aes(lwt, low, color = race)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = race))



# ----------
# by smoke
gg <- ggplot(data, aes(lwt, low, color = smoke)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = smoke))



# ----------
# by ht
gg <- ggplot(data, aes(lwt, low, color = ht)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ht))



# ----------
# by ui --> TRUE
gg <- ggplot(data, aes(lwt, low, color = ui)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ui))



# ----------
# by ptd
gg <- ggplot(data, aes(lwt, low, color = ptd)) + ylab("low") + theme_bw() + geom_point(position = position_jitter(height = 0.02, width = 0))
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = ptd))
