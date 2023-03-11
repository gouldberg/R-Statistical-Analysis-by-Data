# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\sass_tfs")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SASS and TFS
# ------------------------------------------------------------------------------


load("SASS_TFS_data_imputed.Rdata")

str(imputedData)

names(imputedData)




# ------------------------------------------------------------------------------
# Define covariates
# ------------------------------------------------------------------------------


# define covariates for propensity score model

covariateNames <- c(
  "TCHEXPER", #Total years of the principals's experience as a teacher
  # PRIOR to this school year, how many years were you employed in each of the following positions? 
  "A0053",#a. As the principal of THIS school
  "A0054",#As the principal of other schools
  #6d. BEFORE you became a principal, did you hold the following school positions? 
  "A0058",#department head
  "A0059",#(2) Curriculum specialist or coordinator
  "A0060",#(3) Assistant principal or program director
  "A0061",# (4) Guidance counselor
  "A0062",#(5) Library media specialist/Librarian
  "A0063",#6) Athletic coach/athletic director
  "A0065",#did you participate in any district or school training or development program for ASPIRING school principals?
  "TOTTEACH", #how many staff held PART-TIME or FULL-TIME positions 
  "URBANIC", #Urbanicity of school
  "S0104",# Absent 0-9days
  "S0105",#Absent 10-20 days
  "S0106",#Absent 21+ days
  "S0107",#Average daily attendance (number of students categorized)
  "MINENR",#Percent minority students (categorized)
  "MINTCH",#Percent minority teachers (categorized)
  "PGMTYPE", #School type with respect to special programs (6 categories)
  "REGION", #ensus Region (4 categories)
  "S0284", # Eligible K-12 students for free or reduced lunch
  "STU_TCH" ,#Estimated student-teacher ratio
  "LEP_T",#Percent of students with limited English proficiency taught in most recent full week, for teachers with self-contained or departmental classes.
  "PLAN", #Percentage of scheduled school time teacher had for planning during most recent full week of teaching.  
  "PUPILS", #total number of pupins in self-contained or departamentalized classes
  "T0059",#What was your MAIN activity LAST school year?
  "T0106",#How did you earn your regular or standard state certificate or advanced professional certificate in your MAIN teaching assignment field?
  "T0120",#What was your main teaching assignment field LAST school year?
  "T0124",#Did your preparation for teaching include- (1) Coursework in how to select and adapt instructional materials?
  "T0125",#Did your preparation for teaching include- (2) Coursework in learning theory or psychology appropriate to the age of students you teach?
  "T0126",#Did your preparation for teaching include- (3) Your observation of other classroom teaching?
  "T0127",#Did your preparation for teaching include- (4) Feedback on your teaching?
  ##In the past 12 months, have you participated in the following activities RELATED TO TEACHING?
  "T0150",# a. University course(s) taken for recertification or advanced certification 
  "T0153",#d. Individual or collaborative research on a topic of interest to you professionally
  "T0154",#Regularly-scheduled collaboration with other teachers on issues of instruction
  "T0156",#Participating in a network of teachers (e.g., one organized by an outside agency or over the Internet)
  "T0158",#Workshops, conferences or training in which you were the presenter
  "T0208",#At THIS school, what is the total number of students enrolled in the class you taught during your most recent FULL WEEK of teaching?
  #how much control do you think you have IN YOUR CLASSROOM at this school 
  #over each of the following areas of your planning and teaching?     
  "T0293",#a. Selecting textbooks and other instructional materials
  "T0294",#b. Selecting content, topics, and skills to be taught
  "T0295",# c. Selecting teaching techniques
  "T0296",#d. Evaluating and grading students
  "T0297",#e. Disciplining students
  "T0298",#f. Determining the amount of homework to be assigned
  #Do you agree or disagree with each of the following statements? 
  "T0299", #The principal lets staff members know what is expected of them.
  "T0300", #The school administration's behavior toward the staff is supportive and encouraging.
  "T0301", #I am satisfied with my teaching salary.
  "T0303",#e. I receive a great deal of support from parents for the work I do.
  "T0306", #My principal enforces school rules for student conduct and backs me up when I need it.
  "T0307", #The principal talks with me frequently about my instructional practices.
  "T0308", #Rules for student behavior are consistently enforced by teachers in this school, even for students who are not in their classes.
  "T0309", #Most of my colleagues share my beliefs and values about what the central mission of the school should be.
  "T0310",#The principal knows what kind of school he/she wants and has communicated it to the staff.
  "T0311",#There is a great deal of cooperative effort among the staff members.
  "T0312",#In this school, staff members are recognized for a job well done.
  "T0315",#I am satisfied with my class size(s).
  "T0320",#I am generally satisfied with being a teacher at this school.
  "T0321",#Problem - Student tardiness
  "T0322",#Problem-student absenteeism
  "T0324",#Problem-class cutting
  "T0325",#Problem-phys conflicts
  "T0326",#Problem-theft
  "T0327",#Problem-vandalism
  "T0331",#Problem-weapons
  "T0332",#Problem-disrespect for tchrs
  "T0335",#Problem-parental involvement
  "T0336",#Problem-poverty
  "T0337",#Problem-unprepared students
  "T0338",#Problem-student health
  "T0339",#If you could go back to your college days and start over again, would you become a teacher or not?
  "T0340",#How long do you plan to remain in teaching?
  #indicators of data imputed by NCES
  "teachImputed",    #imputed teacher 
  "principalImputed", #imputed principal
  "schoolImputed", #imputed school
  #indicators of data imputed by me
  "missingPrincipal", 
  "missingSchool" )




# ------------------------------------------------------------------------------
# define formulas
# ------------------------------------------------------------------------------

# this includes both individual level and school level covariates

psFormula <- paste(covariateNames, collapse = "+")


psFormula <- formula(paste("Treat ~ ", psFormula, sep = ""))


print(psFormula)




# ------------------------------------------------------------------------------
# Estimate generalized propensity scores by multinomial logit model
# ------------------------------------------------------------------------------

# baseline category logit model to estimate generalized propensity scores

require(VGAM)


ps.model <- vglm(psFormula, weights = imputedData$TFNLWGT, family = multinomial, data = imputedData)


# summary(ps.model)




# ----------
ps <- data.frame(fitted(ps.model))


head(ps)




# ------------------------------------------------------------------------------
# Assess common support
# ------------------------------------------------------------------------------


by(ps, imputedData$Treat, summary)



# evaluate common support with kernel density plots

library(lattice)


lattice.options(default.theme = standard.theme(color = FALSE))




# tiff("Chapter6_figure6-1.tif", res = 600, compression = "lzw", height = 6, width = 15, units = "in")

tmp <- reshape(data.frame(ps,imputedData[,c("CNTLNUM","Treat")]),
               idvar = "CNTLNUM",
               varying = c("noMentor","sameArea","otherArea"),
               v.names = "ps",
               times = c("GPS of No Mentor","GPS of Same Area","GPS of Other Area"),
               direction="long")


head(tmp)


densityplot(~ ps | time, groups = Treat, plot.points = F, auto.key = T, data = tmp,
             ylab = "Generalized Propensity Scores", xlab = "Treatment Conditions")




# ------------------------------------------------------------------------------
# estimate generalized propensity score using generalized boosted modeling
# ------------------------------------------------------------------------------

# implemented by the twang package

library(twang)


set.seed(2016)


# stop criterion: maximum effect size
# number of trees = 10000
# sampw: sampling weights


# IT TAKES TIME !!!:  
load("boost.ps")


# start_time <- Sys.time()

#boost.ps <- mnps(psFormula, data = imputedData,
#                 estimand = "ATE", verbose = FALSE,
#                 stop.method = c("es.max"),
#                 n.trees = 10000,
#                 sampw = imputedData$TFNLWGT)

#end_time <- Sys.time()

#print(end_time - start_time)


summary(boost.ps)




# ----------
# save(boost.ps, file = "boost.ps", compress = T)

# visual inspection of convergence

plot(boost.ps, color = F, plots = 1, pairwiseMax = F)




# ----------
# create a data frame of generealized propensity scores obtained with generealized boosted regression

ps2 <- data.frame(noMentor = boost.ps$psList$noMentor$ps,
                  sameArea = boost.ps$psList$sameArea$ps,
                  otherArea = boost.ps$psList$otherArea$ps)


# assign names
names(ps2) <- c("noMentor", "sameArea", "otherArea")




# ------------------------------------------------------------------------------
# Assess commmon support
# ------------------------------------------------------------------------------

by(ps2, imputedData$Treat, summary)



# ----------
plot(boost.ps, color = F, plots = 2, figureRows = 1)



# ----------
lattice.options(default.theme = standard.theme(color = FALSE))


tmp <- reshape(data.frame(ps2,imputedData[,c("CNTLNUM","Treat")]),
               idvar = "CNTLNUM",
               varying = c("noMentor","sameArea","otherArea"),
               v.names="ps",
               times = c("GPS of No Mentor","GPS of Same Area","GPS of Other Area"),
               direction="long")

head(tmp)


densityplot(~ps | time, groups = Treat, plot.points = F, auto.key = T, data = tmp,
             ylab = "Generalized Propensity Scores", xlab = "Treatment Conditions")




# ------------------------------------------------------------------------------
# Inverse probability treatment weights for propensity score weighting
# ------------------------------------------------------------------------------


imputedData$IPTW <- ifelse(imputedData$Treat == "noMentor", 1 / ps$noMentor, 
                           ifelse(imputedData$Treat == "sameArea", 1 / ps$sameArea, 1 / ps$otherArea))


with(imputedData, by(IPTW, Treat, summary))



# ----------
# Obtain the final weight that is the multiplication of IPTW and sampling weight TFNLWGT

imputedData$IPTW.TFNLWGT <- with(imputedData, IPTW * TFNLWGT)


with(imputedData, by(IPTW.TFNLWGT, Treat, summary))



# ----------
# normalize weights (make them sum to sample size)

imputedData$finalWeightATE <- with(imputedData, IPTW.TFNLWGT / mean(IPTW.TFNLWGT))


with(imputedData, by(finalWeightATE, Treat, summary))




# ------------------------------------------------------------------------------
# calculate the IPTW from the propensity scores obtained with generalized boosting,
# ------------------------------------------------------------------------------

# calculate the IPTW from the propensity scores obtained with generalized boosting, and multiply by the sampling weights

imputedData$finalWeightATE2 <- get.weights(boost.ps, stop.method = "es.max",
                                           estimand = "ATE",
                                           withSampW = TRUE)


# ----------
# normalize weights
imputedData$finalWeightATE2 <- with(imputedData, finalWeightATE2 / mean(finalWeightATE2))




# ----------
# check distribution of weights

with(imputedData, by(finalWeightATE2, Treat, summary))




# ------------------------------------------------------------------------------
# Evaluate covariate balance pairwise
# ------------------------------------------------------------------------------

# evaluate covariate balance with IPTW obtained with multinomial logist cregresson
# function to evaluate covariate balance pairwise

pairwise.balance <- function(condition1, condition2, data, iptw){

  require(twang)
  
  data <- subset(data, Treat == condition1 | Treat == condition2)
  
  data$Treat <- as.numeric(data$Treat == condition1)
  
  balance.iptw <- bal.stat(data = data,
                           vars = covariateNames, #list of the covariates that are used in the treatment assignment model
                           treat.var = "Treat", #treatment assignment indicator
                           w.all = data[,iptw],   #This time, PS weight times Sampling weight is defined
                           get.ks = F,            #Avoid estimating KS statistic
                           sampw = data$TFNLWGT,   #indicate the sampling weight
                           estimand = "ATE",     #Define proper estimand
                           multinom = F)         #Indicate that we make pairwise comparisons,
  
  return( balance.iptw$results) }




# ----------
#obtain balance for all groups pairwise

balance.iptw <- list(pair12 <- pairwise.balance("noMentor", "sameArea", imputedData, "finalWeightATE"), 
                     pair13 <- pairwise.balance("noMentor", "otherArea", imputedData, "finalWeightATE"),
                     pair23 <- pairwise.balance("sameArea", "otherArea", imputedData, "finalWeightATE"))



# obtain a dataset with just standardized effect sizes
std.eff.iptw <- data.frame(abs(balance.iptw[[1]][5]),
                           abs(balance.iptw[[2]][5]),
                           abs(balance.iptw[[3]][5]))

summary(std.eff.iptw)




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# identify covariates/categories with standardized mean difference above 0.1, which will be used in covariance adjustment

unbalanced.covariates <- row.names(std.eff.iptw)[as.logical(apply(std.eff.iptw > 0.1, 1, max))]

unbalanced.covariates <- strsplit(unbalanced.covariates, ":")

unbalanced.covariates <- unique(sapply(unbalanced.covariates, "[", 1))

print(unbalanced.covariates)




# ------------------------------------------------------------------------------
# evaluate covariate balance with the twang function using the generalized propensity scores obtained with generalized boosted regression
# ------------------------------------------------------------------------------

# visual inspection of covariate balance

require(twang)

plot(boost.ps, plots = 3, color = F, pairwiseMax = F)



# ----------
# request a balance table

balance.boost.ps <- bal.table(boost.ps, digits = 2)

summary.balance.boost <- aggregate(std.eff.sz~tmt1+tmt2+stop.method, data=balance.boost.ps, FUN=max)


# identify covariates with standardized mean difference above 0.1, which will be used in covariance adjustment
(balance.above.0.1.gbm <- subset(balance.boost.ps, (std.eff.sz > 0.1 & stop.method == "es.max" )))

unbalanced.covariates2 <- as.character(balance.above.0.1.gbm$var)

unbalanced.covariates2 <- strsplit(unbalanced.covariates2, ":")

unbalanced.covariates2 <- unique(sapply(unbalanced.covariates2, "[", 1))

print(unbalanced.covariates2)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# FUNCTION TO OBTAIN MARGINAL MEAN WEIGHTS through sratification
#  - Arguments:
#  - id = variable with id numbers for dataset
#  - treat = treatment indicator (must be a factor)
#  - ps = data.frame of generalized propensity scores with variable names equal to treatment levels.
#  - numb.strata = number of strata desired

mmws.weights <- function(id, treat, ps, numb.strata) {
  
  all.weights <- data.frame(id)

  for (t in levels(treat)) {
    
    # create strata
    strata <- cut(x = ps[,t], breaks = quantile(ps[,t], prob = seq(0, 1, 1/numb.strata)),
                  labels = 1:numb.strata, include.lowest = T) 
    

    # create treatment by strata table
    treatment <- as.numeric(treat == t)
    
    treat.by.strata <- data.frame(xtabs(~ strata + treatment)) 
    
    # create strata table
    strata.table <- data.frame(table(strata))
    
    names(strata.table)[2] <- "Freq.strata"
    
    
    # ----------
    # merge two tables
    treat.by.strata <- merge(treat.by.strata, strata.table)
    
    print(treat.by.strata)
    
    
    # ----------
    # create marginal mean weights 
    treat.by.strata$mm.weight <- 
      ifelse(treat.by.strata$treatment == 1,
             mean(treatment) * treat.by.strata$Freq.strata / treat.by.strata$Freq,
             (1 - mean(treatment)) * treat.by.strata$Freq.strata / treat.by.strata$Freq)
    
    
    # create full dataset
    data.weight <- data.frame(id, treatment, strata)
    data.weight <- merge(treat.by.strata[,c(1, 2, 5)], data.weight)
    
    
    # ----------
    # convert to zero the weights of individuals that did not receive this level of treatment
    data.weight$mm.weight[data.weight$treatment == 0] <- 0
    data.weight <- data.weight[,3:4]
    names(data.weight)[1] <- paste("W.", t, sep="")
    
    all.weights <- merge(all.weights, data.weight)
  }
  
  # create a single weight
  all.weights$mmws <- apply(all.weights[,-1], 1, sum)
  
  return(all.weights[,c("id","mmws")])
}




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


mmws <- mmws.weights(imputedData$CNTLNUM, imputedData$Treat, ps2, numb.strata = 5)


names(mmws)[1] <- "CNTLNUM"


imputedData <- merge(imputedData,mmws)


by(imputedData$mmws, imputedData$Treat, summary)




# ----------
# multiply by sampling weight and normalize

imputedData$mmwsFinal <- with(imputedData, (mmws * TFNLWGT) / mean(mmws * TFNLWGT)) 

by(imputedData$mmwsFinal, imputedData$Treat, summary)



# ------------------------------------------------------------------------------
# evaluate covariate balance with MMWS
# ------------------------------------------------------------------------------

# obtain balance for all groups pairwise

balance.mmwts <- list(pair12 <- pairwise.balance("noMentor", "sameArea", imputedData, "mmwsFinal"), 
                      pair13 <- pairwise.balance("noMentor", "otherArea", imputedData, "mmwsFinal"),
                      pair23 <- pairwise.balance("sameArea", "otherArea", imputedData, "mmwsFinal"))



# obtain a dataset with just standardized effect sizes
std.eff.mmwts <- data.frame(abs(balance.mmwts[[1]][5]),
                            abs(balance.mmwts[[2]][5]),
                            abs(balance.mmwts[[3]][5]))


summary(std.eff.mmwts)




# ----------
# identify covariates/categories with standardized mean difference above 0.1, which will be used in covariance adjustment
unbalanced.covariates.mmwts <- row.names(std.eff.mmwts)[as.logical(apply(std.eff.mmwts > 0.1, 1, max))]

unbalanced.covariates.mmwts <- strsplit(unbalanced.covariates.mmwts, ":")

unbalanced.covariates.mmwts <- unique(sapply(unbalanced.covariates.mmwts, "[", 1))

print(unbalanced.covariates.mmwts)



# ------------------------------------------------------------------------------
# Estimate ATE with propensity score weighting
# ------------------------------------------------------------------------------

# Fdefine the design
# school ids are provided to control for effects of clustering


require(survey)

design.IPTW <- svydesign(id = ~ SCHCNTL, weights = ~ mmwsFinal, data = imputedData)




# ----------
# create replicate weights for bootstrapping
design.IPTW.boot <- as.svrepdesign(design.IPTW, type = c("bootstrap"), replicates = 1000)



# ----------
# obtain ATT as weighted proportions.

(weightedProportions <- svyby(formula = ~ leftTeaching, by = ~ Treat, design = design.IPTW.boot, 
                              FUN=svymean, covmat=TRUE))


# obtain the weighted proportions with group names to be used in setting up constrats
print(weightedProportions)



# ----------
# calculate pairwise weighted differences between treatment versions
pairwise.ATE <- svycontrast(weightedProportions, 
                            contrasts=list(
                              sameArea.noMentor = c("sameArea:leftTeaching1" = 1, "noMentor:leftTeaching1" = -1), 
                              sameArea.otherArea = c("sameArea:leftTeaching1" = 1, "otherArea:leftTeaching1" = -1), 
                              otherArea.noMentor = c("otherArea:leftTeaching1" = 1, "noMentor:leftTeaching1" = -1)))



# ----------
# Estimate treatmetn effects with regression

model.IPTW <- svyglm("leftTeaching~Treat", design = design.IPTW,
                     family = "quasibinomial")
 

summary(model.IPTW)



# ----------
effects <- unclass(summary(model.IPTW))$coefficients



# ----------
# set up outcome model including covariance adjustment for covariates that did not achieve
# balance within 0.1 standard deviations

outcome.formula <- paste(c("leftTeaching ~ Treat", unbalanced.covariates), collapse = "+")


interactions <- paste("Treat", unbalanced.covariates, sep = ":")


outcome.formula <- paste(c(outcome.formula, interactions), collapse = "+")


print(outcome.formula)



# ----------
# fit outcome model
model.IPTW2 <- svyglm(outcome.formula, design = design.IPTW,
                      family = "quasibinomial")

summary(model.IPTW2)



# extract treatment effects
effects2 <- unclass(summary(model.IPTW2))$coefficients


