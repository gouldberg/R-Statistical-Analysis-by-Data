setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ELS data example career academy
# ------------------------------------------------------------------------------

load("Chapter2_ELS_data_example_career_academy.Rdata")


dim(ELS.data)


str(ELS.data)


# variable names starting with BY were measured in the base year (2002) of the Education Longtudinal Study (ELS)
# variable starting with F1 are composite variables created in 2004 but considered time invariant.


car::some(ELS.data)



# ----------
# To improve clarity, the indicator of participation in a career academy is recoded into the variable treat.
# BYS33K: the indicator of participation in career academy

table(ELS.data$BYS33K)


# create a treatment indicator
ELS.data$treat <- factor(ELS.data$BYS33K)




# ------------------------------------------------------------------------------
# Check missing cases 
# ------------------------------------------------------------------------------

# examine the number of missing cases and its proportion

missing.indicator <- data.frame(is.na(ELS.data))


( propMissing <- apply(missing.indicator, 2, mean) )


sort(round(propMissing, digits = 3), decreasing = T)



# -->
# At maximum, 21.2% of cases are missing values for "bygpared" (Highest reported level of education among parents)
# many variables have missing cases.



# ------------------------------------------------------------------------------
# Create missing indicator
# ------------------------------------------------------------------------------

# create dummy missing value indicators

names(missing.indicator)[propMissing > 0] <- paste(names(ELS.data)[propMissing > 0], "NA", sep="")

names(missing.indicator)




# ----------
# convert dummy missing indicators from logical to numeric variables
for (var in 1:ncol(missing.indicator)) {
  missing.indicator[,var] <- as.numeric(missing.indicator[,var]) }



# ----------
# merge covariate names with missing indicator names
ELS.data <- cbind(ELS.data, missing.indicator[,propMissing > 0])


str(ELS.data)



