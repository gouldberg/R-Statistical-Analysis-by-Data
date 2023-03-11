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
# the distribution of intercepts and slopes:  Catholic vs. Public
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))

boxplot(df_cat$intercept, df_pub$intercept, main = "Intercepts", names = c("Catholic", "Public"))

boxplot(df_cat$slope, df_pub$slope, main = "Slopes", names = c("Catholic", "Public"))



# -->
# It is apparent that Catholic schools on average have larger intercepts and smaller positive slopes than public school.
# Because student's SES is centered within schools, the intercepts estimate the average math achievement score in each school,
# and so average math achievement tends to be higher in Catholic than in public schools

# That the average SES slope is lower in Catholic schools means that SES tends to make less of a difference to student's math achievement
# there than in public schools.



# ----------
df <- rbind(df_cat, df_pub)

formula <- ~ slope + intercept | sector

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c("black", gray(0.7)), pch = c(1, 20))



# ------------------------------------------------------------------------------
# the distribution of intercepts and slopes:  against mean.ses
# ------------------------------------------------------------------------------

formula <- ~ intercept + cses + mean.ses | sector

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c("black", gray(0.7)), pch = c(1, 20))



