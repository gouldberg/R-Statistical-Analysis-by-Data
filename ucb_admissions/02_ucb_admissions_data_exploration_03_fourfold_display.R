setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ------------------------------------------------------------------------------
# Fourfold display
#   - unstandardized:  the area of each quadrant is proportional to the product of the areas shaded dark, 
#     divided by the product of the areas shaded light.
#     the confidence bands have  no interpretation as a test of H0: theta = 1
#
#   - standardize both margin (with preserving the odds ratio)
#     This makes it easier to see the association between admission and sex without being influenced by the overall admission rate or
#     the differential tendency of males and females to apply.
#     The four quadrants will align (overlap) horizontally and vertically when the odds ratio is 1, regardless of the marginal frequencies.
# ------------------------------------------------------------------------------


dat_tab <- margin.table(data, 2:1)


dat_tab



# ----------
# unstandardized fourfold display

fourfold(dat_tab, std = "ind.max")




# ----------
# standardized fourfold display

fourfold(dat_tab, margin = 1)



# standardize both margin

fourfold(dat_tab)




# -->
# The quadrants do not align and the 95% confidence rings around each quadrant do not overlap,
# indicating that the odds ratio differs significantly from 1 -- putative evidence of gender bias.
# The very narrow width of the confidence rings gives a visual indication of the precision of the data.




# ------------------------------------------------------------------------------
# Confidence rings for odds ratio
# ------------------------------------------------------------------------------

summary(loddsratio(dat_tab))


# ----------
exp(0.613 + c(-1, 1) * qnorm(0.975) * 0.06398)

confint(loddsratio(dat_tab, log=FALSE))




# ------------------------------------------------------------------------------
# Fourfold displays for Berkeley admissions data, stratified by department
# ------------------------------------------------------------------------------

# Change admit(row) vs gender(column)  -->  gender(row) vs admit(column)
# We would like to compare admission ratio (columnwise) by gender (row)

dat_tab <- aperm(data, c(2,1,3))

dat_tab




# ----------
# For multiple-strata plots, fourfold() by default adjusts the significance level for multiple testing, using Holm's method provided by p.adjust()

fourfold(dat_tab, mfrow=c(2,3), fontsize = 30)




# --> 
# The more intense shading for Dept. A indicates a sifnificant association
# Surprisingly, this fourfold display shows that for five of the six departments,
# the odds of admission is approximately the same for both men and women applicants.
# Department A appears to differs from the others, with women approximately 2.86 (=(313/19)/(512/89)) times as likely to gain admission.
# This appearance is confirmed by the confidence rings.


# -->
# SIMPSON'S PARADOX
# An overall analysis of a three- (or higher) way table can be misleading.

# Mena nad women apply to different deparments differentially, and in these data women happen to apply in larger numbers to departments that
# have a low acceptance rate.
# The aggregate results are misleading because they falsely assume men and women are equally likely to apply in each field.

# Simpson's paradox occures in a three-way table, [A, B, C], when the marginal association between tow variables,
# A, B collapsing over C, differs in direction from the partial association A, B | C = c(k) at the separate levels of C.
# Strictly speaking, Simpson's paradox would require that for all departments separately the odds ratio theta (k) < 1
# (which occurs for Departments A, B, D, and F) while in the aggretate data theta > 1.

