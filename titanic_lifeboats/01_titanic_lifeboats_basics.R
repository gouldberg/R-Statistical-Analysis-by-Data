setwd("//media//kswada//MyFiles//R//titanic_lifeboats")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Lifeboats on the Titanic
#  - This data tabulates the survivors according to the lifeboats on which they were loaded.
#  - After the disaster, the British Board of Trade launched several inquiries, the most comprehensive of which resulted in the "Report on the Loss of the Titanic"
#    (S.S.) by Lord Mersey (Mersey, 1912). The data frame Lifeboats in vcd contains the data listed on page 38 of that report.
#  - The Titanic was outfitted with 20 boats, half on each of the port and starboard sides, of which 14 were large lifeboats with a capacity of 65, 
#    two were emergency boats designed for 40 persons, and the remaining four were collapsible boats capable of holding 47,
#    a total capacity of 1,178 (considered adequate at that time).
#    Two of the collapsible boats, lashed to the root of the officers' quarters, were ineffectively launched and utilized as rafts after the ship sunk.
#    The report lists the time of launch and composition of the remaining 18 boats according to male passengers, women and children, 
#    and "men of crew", as reported by witnesses.
#  - This data lists a total of 854 in 18 boats, although only 712 were in fact saved.
# ------------------------------------------------------------------------------
data("Lifeboats", package = "vcd")

data <- Lifeboats

data



