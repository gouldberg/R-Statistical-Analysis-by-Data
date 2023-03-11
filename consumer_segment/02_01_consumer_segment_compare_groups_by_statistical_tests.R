# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)



# ---------------------------------------------------------------------------
# Search predictive factors for income by comparing group statistics
# ---------------------------------------------------------------------------
# Testing group means
t.test(income ~ ownHome, data = seg.df)


t.test(income ~ ownHome, data = subset(seg.df, Segment == "Travelers"))


# -->
# The null hypothesis of no difference in income by home ownership is rejected. The data suggests that people who own their homes have higher income.
# But There is not a significant difference in mean income among those Travelers in our data who own homes and who don't.



# ---------------------------------------------------------------------------
# Testing Multiple Group Means: ANOVA
# ---------------------------------------------------------------------------

( seg.aov.own <- aov(income ~ ownHome, data = seg.df) )
( seg.aov.seg <- aov(income ~ Segment, data = seg.df) )
( seg.aov.seg2 <- aov(income ~ Segment + ownHome, data = seg.df) )


anova(seg.aov.own)
anova(seg.aov.seg)
anova(seg.aov.seg2)



# -->
# Segment is a significant predictor but home ownership is not a significant predictor.
# Segment and homeownership are not independent, and the effect is captured sufficiently by segment membership alone.
# Home ownership accounts for little more over and aboe what can be be explained by Segment.



# ---------------------------------------------------------------------------
# Interaction model:  Could it be that home ownership is related to income in some segments but not in others ?
# ---------------------------------------------------------------------------
( seg.aov.seg2_int <- aov(income ~ Segment * ownHome, data = seg.df) )

anova(seg.aov.seg2_int)


# -->
# Again segment is a significant predictor, while home ownership and the interaction of segment with home ownership are not significant.
# In other words, segment membership is again the best predictor on its own.



# ---------------------------------------------------------------------------
# Model comparison
# ---------------------------------------------------------------------------

anova(seg.aov.seg, seg.aov.seg2)


# -->
# Model 2 (both segment and home ownership) is not significantly different in overall fit from Model 1.



# ---------------------------------------------------------------------------
# Visualizing Group Confidece Intervals
# ---------------------------------------------------------------------------
library(multcomp)

glht(seg.aov.seg)



# -->
# The default aov() model has an intercept term (correspinding to the Moving up segment) and all other segments are relative to that.
# This may be difficult for decision makers or clients to understand, so we find it preferable to remove the intercept by adding "-1" to the model formula.



# ----------
( seg.aov.seg_rev <- aov(income ~ -1 + Segment, data = seg.df) )


# With the intercept removed, glht() gives us the mean value for each segment.
( g <- glht(seg.aov.seg_rev) )


par(mar = c(6, 10, 2, 2))

plot(g, xlab = "Income", main = "Average Income by Segment (95% CI)")



    







