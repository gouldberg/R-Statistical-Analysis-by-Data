
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
# Power Analyais
#  - Preparatory steps for the post hoc power calculations
# ------------------------------------------------------------------------------


# extracting the basic information about the model

formula(fm16.5)


fixef(fm16.5)



# -->
# coef for treat.fActive = -2.655




# ----------
# F-test for the treatment effect using the anova() function

anova(fm16.5)


anova(fm16.5, Terms = "treat.f")


anova(fm16.5, L = c("treat.fActive" = 1))



# -->
# F-value = 5.5345



# ------------------------------------------------------------------------------
# Power Analyais
#  - The post hoc power calculations for the treatment effect
# ------------------------------------------------------------------------------

# HERE, we are interested in testing the null hypoothesis H0: beta3 = 0
# against the alternative hypothesis HA: beta3 != 0
# In particular, we want to compute the power for the alternative hypothesis HA(c): beta3 = c (c = -2.65)


# detailed calculation by hand
alpha <- 0.05

df1 <- 1

df2 <- 231

Fvalue <- 5.5345

( Fcrit <- qf(1 - alpha, df1 = df1, df2 = df2, ncp = 0))



( nc <- Fvalue * df1 )

pf(Fcrit, df1 = df1, df2 = df2, ncp = nc, lower.tail = FALSE)




# ----------
# post hoc power calculations using the Pwr() function


nlmeU::Pwr(fm16.5)


nlmeU::Pwr(fm16.5, L = c("treat.fActive" = 1))


nlmeU::Pwr(fm16.5, L = c("treat.fActive" = 1))




# -->
# The post hoc power is a re-expression of the p-value
# Hence, it does not add any new information about the current study.
# It could be of some value if we contemplated repeating the current study with exactly the same sample size.

# The post hoc power would then provide information about the probability of rejecting the null hypothesis
# if the treatment effect were of the same magnitude as the one we have observed.

