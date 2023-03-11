setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# Convert data to data of artificial Poisson data, with the interpolated time varying covariate values
#  - mgcv packages includes a routine tdpois for performing this task
# 
#  - Now consider the pbcseq data, in which the covariates are measured at several times for each patient.
#    The cox.ph family is designed only for the fixed covariate case, but the equivalent Poisson model trick can readily deal with this situation,
#    and can just as well be used with penalized smooth models.
#  - Here let us make the crude assumption that the measurement at an event time is whatever it was the previous time it was measured.
# ------------------------------------------------------------------------------

# ----------
# Function to produce Poisson model data frame
app <- function(x, t, to){
  ## wrapper to approx for calling from apply...
  y <- if (sum(!is.na(x)) < 1) rep(NA, length(to)) else
    approx(t, x, to, method = "constant", rule=2)$y
  if (is.factor(x)) factor(levels(x)[y], levels = levels(x)) else y
}


tdpois <- function(dat, event = "z", et = "futime", t = "day", status="status1", id="id"){

  ## dat is data frame. id is patient id; et is event time; t is observation time; status is 1 for death 0 otherwise;
  ## event is name for Poisson response.
  if (event %in% names(dat)) warning("event name in use")
  require(utils) ## for progress bar
  te <- sort(unique(dat[[et]][dat[[status]] == 1])) ## event times
  sid <- unique(dat[[id]])
  inter <- interactive()
  if (inter) prg <- txtProgressBar(min = 0, max = length(sid), initial = 0,
                                   char = "=", width = NA, title="Progress", style = 3)
  ## create dataframe for poisson model data
  dat[[event]] <- 0; start <- 1
  dap <- dat[rep(1:length(sid), length(te)), ]
  for (i in 1:length(sid)) { ## work through patients
    di <- dat[dat[[id]] == sid[i],] ## ith patient's data
    tr <- te[te <= di[[et]][1]] ## times required for this patient
    ## Now do the interpolation of covariates to event times...
    um <- data.frame(lapply(X=di, FUN=app, t=di[[t]], to=tr))
    ## Mark the actual event...
    if (um[[et]][1] == max(tr) && um[[status]][1] == 1) um[[event]][nrow(um)] <- 1 
    um[[et]] <- tr ## reset time to relevant event times
    dap[start:(start - 1 + nrow(um)), ] <- um ## copy to dap
    start <- start + nrow(um)
    if (inter) setTxtProgressBar(prg, i)
  }
  if (inter) close(prg)
  dap[1:(start-1),]
}



# ----------
# death indicator
pbcseq$status1 <- as.numeric(pbcseq$status == 2)


# Convert to Poisson model data frame
pb <- tdpois(pbcseq)


head(pb)


# ---------
# add factor for eventime
pb$tf <- factor(pb$futime)



# -->
# The resulting pb data frame has 28,380 rows.



