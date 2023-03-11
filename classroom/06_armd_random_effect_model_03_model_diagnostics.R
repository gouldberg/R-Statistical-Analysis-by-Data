
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# residual plots of conditional Pearson residuals
# ------------------------------------------------------------------------------

plot(fm16.2)




# ------------------------------------------------------------------------------
# Plots (and boxplots) of Pearson residuals per time and treatment
# ------------------------------------------------------------------------------


# id = 0.05:  label the residuals larger, in absolute value,
# than the 97.5th percential of the standard normal distribution

plot(fm16.2, resid(., type = "pearson") ~ time | treat.f, id = 0.05)



bwplot(resid(fm16.2, type = "p") ~ time.f | treat.f, 
#       panel = panle.bwxplot2,
       data = armd, pch = "|")




# ------------------------------------------------------------------------------
# Normal Q-Q plots of Pearson residuals and predicted random effects
# ------------------------------------------------------------------------------


qqnorm(fm16.2, ~ resid(.) | time.f)


qqnorm(fm16.2, ~ ranef(.))



# -->
# The resulting Q-Q plot shows slightly curvilinear.
# This could be taken as an indication of nonnormality of the random effects.




# ------------------------------------------------------------------------------
# Outlying conditional Pearson residuals for model
# ------------------------------------------------------------------------------

id <- 0.05


outliers.idx <- 
  within(armd,
         {
           resid.p <- resid(fm16.2, type = "pearson")
           idx <- abs(resid.p) > -qnorm(id/2)
         })


outliers  <- subset(outliers.idx, idx)


nrow(outliers)


outliers$subject




# ------------------------------------------------------------------------------
# Observed and predicted values of visual acuity for selected patients for model
# ------------------------------------------------------------------------------


# augPred() allows obtaining predicted values for the object specified as the first argument
# primary: one-sided formula indicating the covariate at which values the predicted values shoud be computed.
# by default, the arguments become equal to, respectively, the minimum and maximum of the values of the covariate.

# level = 0:1:  population level(Marginal(0)) and (subject level) subj.-spec.(1)

aug.Pred <-
  augPred(fm16.2,                             
          primary = ~time,
          level = 0:1,
          length.out = 2)    


head(aug.Pred)



# ----------
plot(aug.Pred, layout = c(4, 4, 1),
     key = list(lines = list(lty = c(1,2)),
                text = list(c("Marginal", "Subject-specific")),
                columns = 2))



# -->
# The predicted population means decrease linearly in time.
# This is consistent with the trend obesrved.
# According to the assumed structure of the model, the population means are shifted for individual patients
# by subject-specific random intercepts.
# Note that, as a result, the slopes of the individual profiles are the same for all subjects.

# For some patients, the so-obtained predicted individual profiles strongly deviate from the observed ones.

# For instance, for the subjects 4 and 15,
# the predicted individual patterns suggested a decreas of visual acuity over time,
# while the observed values actually increase over time.

