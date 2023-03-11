setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SSOCS
#   - Public use data from the 2007 - 2008 wave of the SSOCS (School Survey on Crime and Safety) to demonstrate propensity score stratification for
#     estimating the average treatment effect (ATE) and average treatment effect on the treated (ATT) of having one full-time security employee
#     (i.e., police officer or security guard) in schools on the proportion of harsh discipline measures administered in response to incidents of crime,
#     violence, and insubordination.
#   - Because the reported numbers of full-time security personnel in the SSOCS schools vary widely (i.e., mean: 1.40, SD: 5.42), the focus here will be on
#     comparing schools with one full-time security employee and schools without any full-time security staff.
#   - The 2007 - 2008 SSOCS was administered to a nationally representative sample of primary, middle, and high schools. Schools were selected for 
#     participation using stratified sampling with explicit stratification by instructional level, locale (i.e., city, suburb, town, and rural), and
#     enrollment size. The complete SSOCS data include responses for 2,560 schools, but removing schools with more than one full-time security
#     employee reduced the sample size to 1,786.
#   - A variable with case weights is proivded with the data set that adjusts for nonresponse bias and differences between population strata sizes and
#     sample strata sizes. Missing data were imputed prior to the data set being made available.
#   - The treatment for this example is defined as a school having one full-time security employee, which could be a full-time school resource officer (SRO),
#     swarn law enforcement officer, or security guard. Schools that dis not employ any security personnel or employed only part-time ones were considered untreated.
#   - The outcome is the proportion of application of harsh discipline for use or possession of weapons, explosives, alcohol, or illegal drugs;
#     physical attacks or fights; and insubordination.
#     Harsh disciplinary practices were defined as removing students from the school with no additional services for the remainder of the year,
#     transferring to specialized schools, or suspending for at leat 5 days.
#     The proportion of harsh discipline is calculated from the responses to Item 22 of the SSOCS as the number of times harsh disciplinary practices were
#     used divided by the number of times harsh and nonharsh practices were used. The mean proportion of times harsh discipline practices were used with
#     this sample of schools is 0.25% (unweighted)
# ------------------------------------------------------------------------------

load(file="Chapter4_ssocs08.Rdata")

dim(SSOCS.data)

str(SSOCS.data)


