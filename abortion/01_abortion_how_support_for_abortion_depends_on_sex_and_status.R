setwd("//media//kswada//MyFiles//R//abortion")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Abortion
#  - 2 * 2 * 2 table of opinions regarding abortion in relation to sex and status of the respondent
# ------------------------------------------------------------------------------
data("Abortion", package = "vcdExtra")

data <- Abortion

data

str(data)


# ----------
# taking support for abortion as the outcome variable
dat <- aperm(data, c(1,3,2))

dat



# ------------------------------------------------------------------------------
# sieve diagram
# ------------------------------------------------------------------------------
cotabplot(data, cond="Status", panel = cotab_sieve, shade = TRUE)

cotabplot(data, cond="Sex", panel = cotab_sieve, shade = TRUE)



# ------------------------------------------------------------------------------
# Visualize this association by fourfold()
# ------------------------------------------------------------------------------
# fourfold dispaly to show the association between sex and support for abortion, stratified by status
dat
fourfold(dat)



# ----------
# fourfold dispaly to show the association between status and support for abortion, stratified by sex
aperm(data, c(2,3,1))
fourfold(aperm(data, c(2,3,1)))



# ------------------------------------------------------------------------------
# Calculate odds ratio
# ------------------------------------------------------------------------------
# Sex and support for abortion, stratified by status
# --> Lo: 2.10   Hi: 0.98
# --> For low status, support for abortion depends on sex
oddsratio(dat, log=FALSE)
confint(oddsratio(dat, log=FALSE))


# Status and support for abortion, stratified by sex
# --> Female: 1.76   Male: 0.82
# --> For female, support for abortion depends on status  
oddsratio(aperm(data,c(2,3,1)), log=FALSE)
confint(oddsratio(aperm(data,c(2,3,1)), log=FALSE))










