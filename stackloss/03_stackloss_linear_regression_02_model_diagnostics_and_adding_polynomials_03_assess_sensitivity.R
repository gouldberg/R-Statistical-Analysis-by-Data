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
# assess sensitivity by adding noise
# ------------------------------------------------------------------------------


summary(dat$Water.Temp)


# add noise to Water.Temp
noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.2)


dat$Water.Temp2 <- dat$Water.Temp + noise



# ----------

mod3_2 <- lm(stack.loss ~ Air.Flow + poly(Water.Temp2, 2), data = dat)


summary(mod3_2)




# ----------
# repeat 1000 times

p <- matrix(0, 1000, 4)


# change noise level sd = 0.01 and sd 0.1

for(i in 1:1000){
  # noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.2)
  noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.4)
  
  dat$Water.Temp2 <- dat$Water.Temp + noise
  
  mod <- lm(stack.loss ~ Air.Flow + poly(Water.Temp2, 2), data = dat)
  
  p[i,] <- broom::tidy(mod)$p.value
}



head(p)



# ratio pvalue < 0.05

myfunc <- function(x) sum(x < 0.05) / 1000


apply(p, 2, myfunc)




# -->
# Water.Temp variable is sensitive to noise level at sd = 0.4 --> problem with predictors

