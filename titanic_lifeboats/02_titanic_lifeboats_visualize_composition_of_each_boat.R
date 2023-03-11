setwd("//media//kswada//MyFiles//R//titanic_lifeboats")

packages <- c("dplyr", "vcd", "MASS", "ggtern")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Lifeboats on the Titanic
# ------------------------------------------------------------------------------
data("Lifeboats", package = "vcd")

data <- Lifeboats

car::some(data)



# ----------
# Label boats with more than 10% men
data$id <- ifelse(data$men / data$total > 0.1, as.character(data$boat), "")



# ------------------------------------------------------------------------------
# Trilinear plots
#  - The trilinear plot (also called a ternary diagram or trinomial plot) is a specialized display for a 3-column contingency table
#    or for three variables whose relative proportions are to be displayed.
#    Individuals may be assigned to one of three diagnostic categories
#  - This plots are featured prominently in Aitchison (1986), who describes statistical models for this type of compositional data.
#    Upton (1976, 1994) uses them in detailed analyses of spatial and temporal changes in British general elections.
#    Wainer (1996) reviews a variety of other uses of trilinear plots and applies them to aid in understanding the distributions of students' achievement
#    in the National Assessment of Educational Progress, making some aesthetic improvements to the traditional form of these plots along the way.
#
# Visualize the composition of the boats by the 3 categories: 
#  - men, women and children, and crew, and according to the launching of the boats from the port or starboard side.
# ------------------------------------------------------------------------------
AES <- aes(x = women, y = men, z = crew, colour = side, shape = side, label = id)

AES


# ggtern(data = data, mapping = AES) + geom_text() + geom_point(size=2) + geom_smooth_tern(method = "lm", alpha = 0.2)
ggtern(data = data, mapping = AES) + theme_rgbw() + geom_point(size=2) + labs(title = "Lifeboats on the Titanic") + labs(T="Women and children") +
  geom_smooth_tern(method="lm", size=1.5, alpha=.2, aes(fill=side)) + geom_text(vjust=1, color="black") +
  theme(legend.position=c(.85, .85))


# -->
# Many of the boats launched from the port side differ substantially from the starboard boats, whose passengers were almost entirely women and children.
# Boat 1 had only 20% (2 out of 10) women and children, while the percentage for boat 3 was only 50% (25 out of 50).
# We highlight the difference in composition of the boats launched from the two sides by adding separte linear regression lines for the relation men ~ women.



# The trilinear plot scales the numbers for each observation to sum to 1.0, so differences in the total number of people on each boat cannot be seen.
# The total number reported loaded is plotted against launch time, with a separte regression line and loess smooth fit to the data for the port and starboard sides.

data$launch <- as.POSIXct(data$launch)

ggplot(data = data, aes(x=launch, y=total, colour=side,  label=boat)) + geom_smooth(method="lm", aes(fill=side), size=1.5) +
  geom_smooth(method="loess", aes(fill=side), se=FALSE, size=1.2) + geom_point() + ylim(c(0,100)) +
  geom_text(vjust=-.5, color="black") + labs(y="Total loaded", x="Launch time")


# --> It seems that the rescue effort began in panic on the port side, with relatively small numbers loaded,
# and, small proportions of women and children.
# But the loading regime on that side improved steadily over time.
# The procedures began more efficiently on the starboard side but the numbers loaded increased only slightly.
# The smoothed loess curves indicate that over time, for each side, there was still a large variability from boat to boat.



# ------------------------------------------------------------------------------
# This may be misleading, in that it does not take into account of the differing capacities of the 18 life boats on the Titanic
#
# Percentage loaded relative to the boat capacity
# ------------------------------------------------------------------------------
data$pctloaded <- data$total / data$cap

ggplot(data = data, aes(x=launch, y=pctloaded, colour=side,  label=boat)) + geom_smooth(method="lm", aes(fill=side), size=1.5) +
  geom_smooth(method="loess", aes(fill=side), se=FALSE, size=1.2) + geom_point() + ylim(c(0,1)) +
  geom_text(vjust=-.5, color="black") + labs(y="Total loaded", x="Launch time")





