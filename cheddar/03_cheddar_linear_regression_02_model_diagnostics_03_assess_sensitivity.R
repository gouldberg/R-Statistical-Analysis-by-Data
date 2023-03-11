# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# assess sensitivity by adding noise
# ------------------------------------------------------------------------------


# add noise to Lactic
noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.01)


dat$Lactic2 <- dat$Lactic + noise



# ----------

mod2 <- update(mod1, . ~ . - Lactic + Lactic2)


summary(mod2)




# ----------
# repeat 1000 times

p <- matrix(0, 1000, 3)


# change noise level sd = 0.01 and sd 0.1

for(i in 1:1000){
  # noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.01)
  noise <- rnorm(n = nrow(dat), mean = 0, sd = 0.1)
  
  dat$Lactic2 <- dat$Lactic + noise
  
  mod <- lm(taste ~ H2S + Lactic2, data = dat)

  p[i,] <- broom::tidy(mod)$p.value
}



head(p)



# ratio pvalue < 0.05

myfunc <- function(x) sum(x < 0.05) / 1000


apply(p, 2, myfunc)




# -->
# Lactic variable is sensitive to noise level at sd = 0.1 --> problem with predictors

