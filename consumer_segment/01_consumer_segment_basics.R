# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
#   - For this example, we are offering a subscription-based service (such as cable television or membership in a warehouse club) and
#     have collected data from N = 300 respondents on age, gender, income, number of children, whether they own or rent their homes,
#     and whether they currently subscribe to the offered service or not.
#   - Each respondent has been assigned to one of four consumer segments: "Suburb mix", "Urban hip", "Travelers", or "Moving up"
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)



# ------------------------------------------------------------------------------
# basic analysis:  group statistics
# ------------------------------------------------------------------------------

by(seg.df$income, seg.df$Segment, mean)


# aggregate() understands formula models and produces a reusable, indexable object with its results
aggregate(seg.df$income, list(seg.df$Segment), mean)


boxplot(income ~ Segment, data = seg.df, yaxt = "n", ylab = "Income ($k)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, "k", sep = ""), las = 1)



# ----------
# list() is used to give multiple indices
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)


# aggregate() understands formula model
aggregate(income ~ Segment + ownHome, data = seg.df, mean)


aggregate(income ~ Segment + ownHome + subscribe, data = seg.df, mean)




# ---------------------------------------------------------------------------
# report mean by group
# ---------------------------------------------------------------------------

seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


seg.summ(seg.df, seg.df$Segment)

