setwd("//media//kswada//MyFiles//R//donner")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Donner
#  - In April-May of 1946 (three years before the California gold rush), the Donner and Reed families set out for California from the American Mid-west
#    in a wagon train to seek a new life and perhaps their fortune in the new American frontier. By mid-July, a large group had reached a site in present-day Wyoming;
#    George Donner was elected to lead what was to be called the "Donner Party", which eventually numbered 87 people in 23 wagons, along with their oxen, cattle,
#    horses, and worldly posessions.
#  - They were determined to reach California as quickly as possible. Lansford Hastings, a self-proclaimed trailblazer (retrospectively, of dubious distinction),
#    proposed that the party follow him through a shorter path through the Wasatch Mountains. Their choice of "Hasting's Cutoff" proved disastrous: Hastings had never
#    actually crossed that route himself, and the winter of 1846 was to be one of the worst on record.
#  - In October, 1846, heavy snow stranded them in the eastern Sierra Nevada, just to the east of a pass that bears their name today. The party made numerous attempts
#    to seek rescue, most turned back by blizzard conditions. Relief parties in March-April 1847 rescued 40, but discovered grisly evidence that those who survived
#    had cannibalized those who died.
#  - Donner data set lists 90 people in the Donner Party by name, together with age, sex , survived (0/1), and the date of death for those who died.
#
#  - Most historical sources count the number in the Donner Party at 87 or 89.  An exact accounting of the members of the Donner Party is difficult, because:
#     - (a) several people joined the party in mid-route, at Fort bridger and in the Wasatch Mountains
#     - (b) several rode ahead to search for supplies and one (Charles Stanton) brought two more with him (Luis and Salvador)
#     - (c) 5 people died before reaching the Sierra Nevada mountains
# ------------------------------------------------------------------------------
data("Donner", package = "vcdExtra")

dim(Donner)
str(Donner)


car::some(Donner)


data <- Donner

data$survived <- factor(Donner$survived, labels = c("no", "yes"))


