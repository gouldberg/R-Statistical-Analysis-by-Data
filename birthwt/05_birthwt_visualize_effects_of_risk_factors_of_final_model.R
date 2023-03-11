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

data$low <- factor(data$low, levels = c(0, 1))
data$bwt <- NULL
data$ptl <- NULL



# ------------------------------------------------------------------------------
# Full-model plots
#
# Disply of fitted values for a binary regression model with one numeric predictor, conditioned by zero or many co-factors
# Check baseline (no risk factors) and risk factors impact
# ------------------------------------------------------------------------------
levels(data$smoke) <- c("-", "smoke")
levels(data$ht) <- c("-", "ht")
# levels(data$ui) <- c("-", "ui")
levels(data$ptd) <- c("-", "ptd")


btw.final <- glm(low ~ age + race + smoke + ht + ptd, data = data, family = binomial)
btw.final2 <- glm(low ~ lwt + ht + ptd, data = data, family = binomial)



# ----------
# Fitted log odds of death in ICU with 1 standard error confidence bands
# by default, binreq_plot() uses the first numeric predictors as the horizontal variable
vcd::binreg_plot(btw.final, type = "link", conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Log odds (low)",  ylim = c(-7, 4))


vcd::binreg_plot(btw.final2, type = "link", conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Log odds (low)",  ylim = c(-7, 4))

