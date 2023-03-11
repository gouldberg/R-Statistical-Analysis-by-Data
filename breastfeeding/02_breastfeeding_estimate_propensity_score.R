setwd("//media//kswada//MyFiles//R//breastfeeding")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  breastfeeding
# ------------------------------------------------------------------------------

load(file="//media//kswada//MyFiles//references//PracticalPropensityScoreMethodsUsingR//Chapter5//Chapter5_data_breastfeeding_example.rData")


str(data)

glimpse(data)



# ------------------------------------------------------------------------------
# preparation
# ------------------------------------------------------------------------------

# define propensity score formula
covariateNames <- c(
  "C0005300", #RACE OF CHILD (MOTHER'S RACIAL/ETHNIC COHORT FROM SCREENER)                CRACE          
  "C0005400", #SEX OF CHILD                                                         
  "C0270300", #NUMBER OF JOBS HELD BY MOTHER IN 4TH QTR BEFORE BIRTH OF CHILD             NJOBS01               
  "C0270600", #USUAL HOURS WORKED BY MOTHER AT ALL JOBS IN 4TH QTR BEFORE BIRTH OF CHILD  NHRJBS01       
  "C0270700", #USUAL EARNINGS OF MOTHER AT ALL JOBS IN 4TH QTR BEFORE BIRTH OF CHILD      EARNJB01       
  "C0271800", # COLLECTIVE BARGAINING SET MOTHER'S MAIN JOB WAGES, 4TH QTR BEFORE BIRTH OF CHILD  UNONMN01
  "C0328000", #LENGTH OF GESTATION OF CHILD IN WEEKS                                             PST0002        
  "C0328400", #CHILD DELIVERED BY CESAREAN SECTION?                                          PST0007        
  "C0329000", # DAYS MOTHER STAYED IN HOSPITAL AT BIRTH OF CHILD                             PST0013        
  "C0329100", # DAYS CHILD STAYED IN HOSPITAL AFTER DELIVERY                                 PST0014        
  "R0000700",    #COUNTRY OF BIRTH  OF MOTHER
  "R0618300",    #PROFILES ARMED FORCES QUALIFICATION TEST (AFQT) PERCENTILE SCORE - REVISED 1989 AFQT-2 
  "C0270200", #weeks after birth that mother returned to work (restricted to 12 weeks or less)
  "motherAge", #age of mother at child's birth
  "classWorker", #CLASS OF WORKER AT CURRENT JOB/MOST RECENT JOB
  "familySize" , #FAMILY SIZE
  "highestGrade" , #HIGHEST GRADE COMPLETED AS OF MAY 1 SURVEY YEAR (REVISED)
  "jobsEver" , #NUMBER OF DIFFERENT JOBS EVER REPORTED AS OF INTERVIEW DATE
  "maritalStatus" , #MARITAL STATUS (COLLAPSED)
  "residenceRegion", #REGION OF CURRENT RESIDENCE
  "ruralUrban"  , #IS R'S CURRENT RESIDENCE URBAN/RURAL?
  "totalWelfare",  # TOTAL AMOUNT AFDC FOOD STAMPS OR OTH WELFARE/SSI RECEIVED DURING CAL YR 
  "weeksWorked", #NUMBER OF WEEKS WORKED IN PAST CALENDAR YEAR
  "hoursPerWeek", #hours per week worked in the last calendar year
  "maternityLeave", #FRINGE BENEFITS CURRENT JOB/MOST RECENT JOB - MATERNITY/PATERNITY LEAVE 
  "flexibleSchedule", #job allows flexible schedule
  "argueChores",#FREQUENCY R & HUSBAND/PARTNER ARGUE ABOUT-CHORES & RESPONSIBILITIES
  "dentalInsurance", #company provides dental insurance
  "lifeInsurance", #company provides life insurance
  "profitSharing", #company provides profit sharing
  "retirement", #company provided retirement plan
  "training") #company provided training opportunities


# ----------
# add missing value indicators to propensity score model
covariateNamesNA <- paste(covariateNames, "NA", sep = "")


# restrict missing indicators to covariates with at least 5% of missing data
covariateNamesNA <- covariateNamesNA[apply(data[,covariateNamesNA], 2, mean) >= 0.05]


# merge covariate names with missing indicator names
covariateNames <- c(covariateNames, covariateNamesNA)




# ------------------------------------------------------------------------------
# Estimate propensity score
# ------------------------------------------------------------------------------
# obtain the propensity score formula   
psFormula <- paste(covariateNames, collapse = "+")

psFormula <- formula(paste("childCare ~ ", psFormula, sep = ""))

print(psFormula)



# ----------
# estimate propensity scores with logistic regression
psModel <- glm(psFormula, data, family = binomial())

data$logitPScores <- log(fitted(psModel) / (1 - fitted(psModel)))



# ----------
# estimate propensity scores with random forests
library(party)
set.seed(2015)

mycontrols <- cforest_unbiased(ntree = 10000, mtry = 5)

mycforest <- cforest(psFormula, data = data, controls = mycontrols)

# obtain a list of predicted probabilities
predictedProbabilities <- predict(mycforest, type = "prob")


# organize the list into a matrix with two columns for the probability of being in treated and control groups.
# keep only the second column, which are the propensity scores.
pScores <- matrix(unlist(predictedProbabilities),,2,byrow=T)[,2]


# convert propensity scores to logit
data$logitPScores <- log(pScores / (1 - pScores))



# ------------------------------------------------------------------------------
# Evaluate common support with plots
# ------------------------------------------------------------------------------
# tiff("Chapter5_figure5-1.tif", res=600, compression = "lzw", height=6, width=15, units="in")

hist(data$logitPScores[data$childCare == 0], density = 10, angle = 45, main = "Propensity Scores", xlab = "Shaded = Untreated | Gray = Treated") 

hist(data$logitPScores[data$childCare==1], col = gray(0.4, 0.25), add = T) 

# dev.off()


library(lattice)
# tiff("Chapter5_figure5-2.tif", res=600, compression = "lzw", height=6, width=15, units="in")
bwplot(logitPScores ~ childCare, data = data, ylab = "Propensity Scores", auto.key = TRUE)
# dev.off()


# obtain descriptive statistics
summary(data$logitPScores[data$childCare == 1])
summary(data$logitPScores[data$childCare == 0])


# -->
# Common support is potentially adequate to estimate the ATT with matching methods,
# because the distribution of the treated is contained within the distribution of the untreated, and therefore an adequate match
# could be found for every treated observation.

# Also indicating that estimating the ATE using propensity score matching with these data may be difficult because there are areas of the
# distribution of the untreated without any treated cases nearby, which could result in poor matching.

# Therefore, the ATT of a mother working for a company that provides or subsidizes child care will be estimated.
# In applications of propensity score matching, the ATT is more commonly estimated than the ATE.


