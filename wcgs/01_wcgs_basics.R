setwd("//media//kswada//MyFiles//R//wcgs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wcgs
#   - What might affect the chance of getting heart disease ?
#     One of the earliest studies addressing this issue started in 1960 and used 3154 healthy men, aged from 39 to 59,
#     from the San Francisco area. At the start of the study, all were free of heart disease.
#     Eight and a half years later, the study recorded whether these men now suffered from heart disease along with many other variables
#     that might be related to the change of developing this disease.
# ------------------------------------------------------------------------------

data("wcgs", package = "faraway")

str(wcgs)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

summary(wcgs[,c("chd", "height", "cigs")])



# ----------
par(mfrow=c(1,1))
plot(height ~ chd, wcgs)



# ----------
# we add jitter
# Again we can see the similarity in the distribution.
wcgs$y <- ifelse(wcgs$chd == "no", 0, 1)
plot(jitter(y, 0.1) ~ jitter(height), wcgs, xlab = "Height", ylab = "Heart Disease", pch = ".")



# ----------
# More informative plot by dodge option that ensures that the tow histrograms are interleaved.
ggplot(wcgs, aes(x = height, color = chd)) + geom_histogram(position = "dodge", binwidth = 1)

ggplot(wcgs, aes(x = cigs, color = chd)) + geom_histogram(position = "dodge", binwidth = 5, aes(y = ..density..))

ggplot(wcgs, aes(x = height, y = cigs)) + geom_point(alpha=0.2, position = position_jitter()) + facet_grid(~ chd)



