setwd("//media//kswada//MyFiles//R//visual_acuity")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Visual Acuity
# ------------------------------------------------------------------------------
data("VisualAcuity", package = "vcd")

data <- VisualAcuity

car::some(data)


# Convert frequency data frame to table form
dat <- xtabs(Freq ~ right + left + gender, data = VisualAcuity)

dimnames(dat)[1:2] <- list(c("high", 2, 3, "low"))

names(dimnames(dat))[1:2] <- paste(c("Right", "Left"), "eye grade")

dat


structable(aperm(dat))



# ------------------------------------------------------------------------------
# Check association pattern by sieve diagram
# ------------------------------------------------------------------------------
# Vision classification for 7,477 women in Royal Ordnance factories.
# The high frequenceies in the diagonal cells indicate the main association, but a subtler pattern also appears in the symmetric off-diagonal cells.
sieve(dat[, , "female"], shade = TRUE)


# -->
# The diagonal cells show the obvious: people tend to have the same visual acuity in both eyes, and there is strong lack of independence.
# The off diagonal cells show a more subtle pattern that suggests symmetry -- the cells below the diagonal are approximately equally dense at the 
# corresponding cells above the diagonal.

# Moreover, the relatively consistent pattern on the diagonals, away from teh main diagonals suggests that the asoociation may be
# explained in terms of the difference in visual acuity between the two eyes.



# ------------------------------------------------------------------------------
# Conditioning by gender
# ------------------------------------------------------------------------------
# This generate same pattern diagram, including both of male and female
sieve(Freq ~ right + left, data = data, shade = TRUE)



# ----------
# conditioning by gender
# set_varnames argument relabels the variable names
sieve(Freq ~ right + left | gender, data = data, shade = TRUE, set_varnames = c(right = "Right eye grade", left = "Left eye grade"))


# -->
# The relative sizes of the blocks for the conditioning variable (gender) show the much larger number of women than men in this data.
# Within each block, color and density of the box rules shows the association of left and right acuity, and it apears that the pattern for men
# is similar to that observed for women.



# ----------
# an alternative way of visualizing stratfied data is a coplot or conditioning plot
# But the distribution of the conditioning variable is not shown.
cotabplot(dat, cond = "gender", panel = cotab_sieve, shade = TRUE)


