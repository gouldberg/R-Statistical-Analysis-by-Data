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

# We perform a backward stepping procedure (the default direction)
seg.aov.step <- step(aov(income ~ ., data = seg.df))

anova(seg.aov.step)


# -->
# The best model is "income ~ Segment"



