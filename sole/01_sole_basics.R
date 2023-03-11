setwd("//media//kswada//MyFiles//R//sole")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sole
#   - The data (available in Dixson, 2003) are measurements of density of eggs per square metre of sea surface in each of 4 identifiable egg
#     developmental stages, at each of a number of sampling stations in the Bristol channel on the west coast of England.
#     The samples were taken during 5 cruises spaced out over ths spawning season.
#   - A data frame with 7 columns and 1575 rows. The columns are:
#        - la:  latitude of sampling station
#        - lo:  longitude of sampling station
#        - t:  time of sampling station: actually time of midpoint of the cruise on which this sample was taken. Measured in Julian days (days since January 1st).
#        - eggs:  egg density per square metre of sea surface.
#        - stage:  to which of 4 stages the sample relates.
#        - a.0:  lower age limit for the stage (i.e. age of youngest possible egg in this sample).
#        - a.1:  upper age limit of this stage (i.e. age of oldest possible egg in sample).
# ------------------------------------------------------------------------------

data("sole", package = "gamair")

str(sole)

head(sole)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

# There are many zeros ...
lattice::dotplot(as.matrix(sole$eggs), data = sole)
                 


# ----------
lattice::xyplot(la ~ lo | stage, data = sole, cex = sqrt(sole$eggs))
