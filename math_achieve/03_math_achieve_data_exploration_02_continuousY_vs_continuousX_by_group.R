setwd("//media//kswada//MyFiles//R//math_achieve")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathAchieve
# ------------------------------------------------------------------------------

data("MathAchieve", package = "nlme")

dim(MathAchieve)

str(MathAchieve)

car::some(MathAchieve)



# ----------
data("MathAchSchool", package = "nlme")

dim(MathAchSchool)

str(MathAchSchool)

car::some(MathAchSchool)



# ----------
names(MathAchieve) <- tolower(names(MathAchieve))
names(MathAchSchool) <- tolower(names(MathAchSchool))


Temp <- MathAchieve %>% group_by(school) %>% summarize(mean.ses = mean(ses))
Temp <- merge(MathAchSchool, Temp, by = "school")
car::brief(Temp)


# ----------
HSB <- merge(Temp[, c("school", "sector", "mean.ses")], MathAchieve[, c("school", "ses", "mathach")], by = "school")


# ----------
HSB$cses <- with(HSB, ses - mean.ses)
car::brief(HSB)



# ------------------------------------------------------------------------------
# Select samples
# ------------------------------------------------------------------------------

# We select samples of 20 public and 20 Catholic schools

set.seed(12345)


n_sample <- 20

cat <- with(HSB, sample(unique(school[sector == "Catholic"]), n_sample))

( Cat.samp <- HSB[is.element(HSB$school, cat),] )

dim(Cat.samp)



# ----------
pub <- with(HSB, sample(unique(school[sector == "Public"]), n_sample))

( Pub.samp <- HSB[is.element(HSB$school, pub),] )

dim(Pub.samp)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

library(lattice)


xyplot(mathach ~ cses | school, data = Cat.samp, main = "Catholic",
       panel = function(x, y){
         panel.points(x, y)
         panel.lmline(x, y, lty = 2, lwd = 2, col = "darkgray")
         panel.loess(x, y, span = 1, lwd = 2)
       }
)


xyplot(mathach ~ cses | school, data = Pub.samp, main = "Public",
       panel = function(x, y){
         panel.points(x, y)
         panel.lmline(x, y, lty = 2, lwd = 2, col = "darkgray")
         panel.loess(x, y, span = 1, lwd = 2)
       }
)



# -->
# There is a weak positive relationship between math achievement and SES in most Catholic schools,
# although there is variation among schools.
# In some schools, the slope of the regression lne is near zero or even negative.
# There is also a positive relationship between the two variables.



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

library(car)

catpub <- rbind(Cat.samp, Pub.samp)


formula <- mathach ~ cses | sector

scatterplot(formula, data = catpub, pch = c(1,20), cex = 0.3, col = c(gray(0.7), "black"))





