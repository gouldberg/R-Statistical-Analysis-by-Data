setwd("//media//kswada//MyFiles//R//suicide")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
#   - data on suicide rates in West Germany classified by sex, age, and method of suicide used.
#   - The data, from Heuer (1979) have been discussed by Friendly, van der Heijden and de Leeuw, and others.
#   - The original 2 * 17 * 19 table contains 17 age groups from 10 to 90 in 5-years steps and 9 categories of suicide method, 
#     contained in the frequency data frame Suicide in vcd, with table variables sex, age, and method.
#     To avoid extremely small cell counts and cluttered displays, this example uses a reduced table in which age groups are combined in the variable age.group,
#     a factor with 15-year intervals except for the last interval, which includes ages 70-90;
#     the methods "toxic gas" and "cooking gas" were collapsed (in the variable method2) giving the 2 * 5 * 8 table.
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data

car::some(data)



# ------------------------------------------------------------------------------
# Interactive coding of sex and age.group
# ------------------------------------------------------------------------------

data <- within(data, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})


car::some(data)


( tab <- xtabs(Freq ~ age_sex + method2, data = data) )
