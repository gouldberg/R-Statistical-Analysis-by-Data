
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

plot(fm16.4)




# ------------------------------------------------------------------------------
# Plots (and boxplots) of Pearson residuals per time and treatment
# ------------------------------------------------------------------------------


# id = 0.05:  label the residuals larger, in absolute value,
# than the 97.5th percential of the standard normal distribution

plot(fm16.4, resid(., type = "pearson") ~ time | treat.f, id = 0.05)



bwplot(resid(fm16.4, type = "p") ~ time.f | treat.f, 
       #       panel = panle.bwxplot2,
       data = armd, pch = "|")




# ------------------------------------------------------------------------------
# Normal Q-Q plots of Pearson residuals and predicted random effects
# ------------------------------------------------------------------------------


qqnorm(fm16.4, ~ resid(.) | time.f)


qqnorm(fm16.4, ~ ranef(.))



# -->
# slightly closer to a straight line than the former.



# ------------------------------------------------------------------------------
# Outlying conditional Pearson residuals for model
# ------------------------------------------------------------------------------

id <- 0.05


outliers.idx <- 
  within(armd,
         {
           resid.p <- resid(fm16.4, type = "pearson")
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
  augPred(fm16.4,                             
          primary = ~time,
          level = 0:1,
          length.out = 2)    


head(aug.Pred)



# ----------
plot(aug.Pred, layout = c(4, 4, 1),
     key = list(lines = list(lty = c(1, 2)),
                text = list(c("Marginal", "Subject-specific")),
                columns = 2))



# -->
# slopes vary by subject
# predicted individual profles follow more closely the observed values and capture,
# e.g., increasing trends in time.

