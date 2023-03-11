# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data



# ------------------------------------------------------------------------------
# mosaic display:  Two-way tables
# ------------------------------------------------------------------------------

# Interactive coding of sex and age.group
data <- within(data, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})


car::some(data)


( tab <- xtabs(Freq ~ age_sex + method2, data = data) )


mosaic(tab, shade = TRUE, labeling = labeling_residuals)




# ------------------------------------------------------------------------------
# mosaic display:  3-way tables
# ------------------------------------------------------------------------------

( tab2 <- xtabs(Freq ~ sex + age.group + method2, data = data) )


mosaic(tab2, shade = TRUE, labeling = labeling_residuals)



# -->
# In both mosaic displays, we can see clearly independence of [AgeSex][Method] is rejected...
