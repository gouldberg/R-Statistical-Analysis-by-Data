setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gala")



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# Add additional term with linearly combined data
# ------------------------------------------------------------------------------


( gala$Adiff <- gala$Area - gala$Adjacent )


lmod_tmp <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent + Adiff, data = gala)

# lmod_tmp <- update(lmod, . ~ . + Adiff)


summary(lmod_tmp)

summary(lmod)




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# Add additional term with linearly combined data
# ------------------------------------------------------------------------------


( gala$Adiffe <- gala$Adiff + 0.001 * runif(30) - 0.5 )


lmod_tmp2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent + Adiff + Adiffe, data = gala)

# lmod_tmp2 <- update(lmod, . ~ . + Adiff + Adiffe)


summary(lmod_tmp2)

summary(lmod)



# -->
# Note that Adjacent is not significant and almost same coef with Adiffe
# Area is tremendously large negative coefficients



# ----------
# Here collinearity is detected

vif(lmod_tmp2)

vif(lmod)


