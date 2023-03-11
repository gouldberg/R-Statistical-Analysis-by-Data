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
# linear regression by each school
# ------------------------------------------------------------------------------

library(nlme)


# We fit the regression of math achievement scores on centered socioeconomic status for each school

cat.list <- lmList(mathach ~ cses | school, subset = sector == "Catholic", data = HSB)


pub.list <- lmList(mathach ~ cses | school, subset = sector == "Public", data = HSB)


summary(cat.list[[1]])

brief(cat.list[[1]])




# ----------
# extract intercepts and slopes
intercepts_cat <- sapply(cat.list, coef)[1,]
intercepts_cat <- data.frame(school = names(intercepts_cat), intercept = intercepts_cat)

slopes_cat <- sapply(cat.list, coef)[2,]
slopes_cat <- data.frame(school = names(slopes_cat), slope = slopes_cat)


intercepts_pub <- sapply(pub.list, coef)[1,]
intercepts_pub <- data.frame(school = names(intercepts_pub), intercept = intercepts_pub)

slopes_pub <- sapply(pub.list, coef)[2,]
slopes_pub <- data.frame(school = names(slopes_pub), slope = slopes_pub)


df_cat <- intercepts_cat %>% left_join(., slopes_cat, by = "school") %>% left_join(., HSB %>% dplyr::select(school, mean.ses, cses), by = "school")

df_pub <- intercepts_pub %>% left_join(., slopes_pub, by = "school") %>% left_join(., HSB %>% dplyr::select(school, mean.ses, cses), by = "school")


df_cat <- df_cat %>% mutate(sector = "Catholic")

df_pub <- df_pub %>% mutate(sector = "Public")


head(df_cat)
