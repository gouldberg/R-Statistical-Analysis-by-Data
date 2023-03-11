# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)


names(data)



# ------------------------------------------------------------------------------
# data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(data)


psych::describe(data)




# ----------
# The mean number of logins per examinee is 2.50 and the maximum is 25.13 during a time period of 3 months.
# Because the distribution of logins per examinee is highly skewered,
# a log transformation is applied to the number of logins per examinee to reduce skewness

summary(data$loginsPerExaminee)

summary(data$logLoginsPerExaminee)




# ------------------------------------------------------------------------------
# Plot treatment measure: total logins per examinee and the log of total logins per examinee
# ------------------------------------------------------------------------------

# histogram to compare the distribution of the original and log-transformed treatment
# indicating that the log transofrmation succeeds in reducint nonnoarmality

with(data, hist(loginsPerExaminee, density = 10, angle = 45, main="",
                xlim=c(-10,26), ylim=c(0,300), xlab="Shaded = Treatment doses | Gray = Log Treatment Doses ") )

with(data,
     hist(logLoginsPerExaminee, col = gray(0.4,0.25), 
          xlim=c(-10,26), ylim=c(0,250), add=T) )



