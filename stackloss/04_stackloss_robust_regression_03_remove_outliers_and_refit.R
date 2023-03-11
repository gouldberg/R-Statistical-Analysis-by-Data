# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)





# ------------------------------------------------------------------------------
# Remove outliers and refit
# ------------------------------------------------------------------------------


mod_del <- lm(stack.loss ~ Air.Flow + poly(Water.Temp, 2), data = dat[c(-4,-3),])


summary(mod_del)




# ----------
coef(mod3)


coef(rlmod)


coef(ltsmod)


coef(mod_del)




# ----------

par(mfrow = c(2,2))

plot(mod_del)


car::influenceIndexPlot(mod_del)

