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
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------

set.seed(123)


ltsmod <- ltsreg(stack.loss ~ Air.Flow + poly(Water.Temp, 2), data = dat)


ltsmod



# ----------
coef(mod3)


coef(rlmod)


coef(ltsmod)




# ----------
ltsmod$bestone



# -->
# 6,9,12,18



