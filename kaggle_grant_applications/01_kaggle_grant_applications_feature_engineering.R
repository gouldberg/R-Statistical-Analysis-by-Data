rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("plyr", "dplyr", "caret", "lubridate")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  kaggle grant applications
#   - These data are from a 2011 Kaggle competition sponsored by the University of Melbourne where there was interest in predcting whether or not a
#     grant application would be accepted. Since public funding of grants had decreased over time, triaging grant applications based on their
#     likelihood of success could be important for estimating the cmount of potential funding to the university.
#     In addition to predicting grant success, the university sought to understand factors that were important in predicting success.
#
#   - The winning entry achieved an area under the ROC curve of 0.968 on the test set.
#
#   - This dataset includes 249 features (or predictors). Participants should use these variables to predict the target variable (or outcome), "Grant Status".
#     A grant status of 1 represents a successful grant application, while a grant status of 0 represents an unsuccessful application.
#     The training dataset, which participants use to build their models, is unimelb_training.csv. 
#     It contains 8,707 grant applications from late 2005 to 2008. 
#     The test dataset, unimelb_test.csv, contains 2,176 grant applications from 2009 to mid 2010. The grant status variable is withheld from the test dataset.
#   - Predictions should take the same format as unimelb_example.csv
#     (a CSV file with 2,176 rows, a grant application ID in the first column and a probability of success - between 0 and 1 - in the second column). 
#
#   - The role of each individual listd on the grant. The total number of indivisuals listed on the grant ranged from 1 to 14.
#     The several characteristics of each individual on the grant.
#     Many fields are broken down for each individual involved in the grant. Since there could be as many as 14 individuals associated with a grant,
#     there are a large number of columns for a grant, many of which have no data.
#
#   - The university has provided the following features:
#       - Sponsor Code: an ID used to represent different sponsors
#       - Grant Category Code: categorization of the sponsor (e.g. Australian competitive grants, cooperative research centre, industry)
#       - Contract Value Band: the grant's value
#       - Start Date: the date the grant application was submitted
#       - RFCD Code: research fields, courses and disciplines classification (see definitions)
#       - RFCD Percentage: if there are several RFCD codes that are relevant to a project
#       - SEO Code: socio economic objective classification (see definitions)
#       - SEO Percentage: if there are several SEO codes that are relevant to a project
#       - Person ID: the investigator's unique ID
#       - Role: the investigator's role in the study
#       - Year of Birth: the investigator's year of birth (rounded to the nearst five year interval)
#       - Country of birth: the investigator's country of birth (often aggregated to by-continent)
#       - Home Language: the investigator's native language (classified into English and Other)
#       - Dept No: the investigator's department
#       - Faculty No: the investigator's faculty
#       - Grade Level: the investigator's level of seniority
#       - No. of years in Uni at time of grant: the number of years the investigator had been at the University of Melbourne when the grant application was made
#       - Number of Successful Grant: the number of successful grant applications the investigator had made
#       - Number of Unsuccessful Grant: the number of unsuccessful grant applications the investigator had made
#       - A*: number of A* journal articles
#       - A: number of A journal articles
#       - B: number of B journal articles
#       - C: number of C journal articles
# ------------------------------------------------------------------------------
library(doMC)

cores <- 20
registerDoMC(cores)



# ------------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------------
# Read in the data in it's raw form. Some of the column headings do not convert to proper R variable names, so many will contain dots,
# such as "Dept.No" instead of "Dept No"

raw <- read.csv("/media/kswada/MyFiles/data/kaggle_grant_applications/data_original/unimelb_training.csv")

dim(raw)

str(raw)



# ----------
# check the target response variable
table(raw$Grant.Status)



# ----------
# check counts of na
counts_na <- function(x) sum(is.na(x))
apply(raw, MARGIN=2, FUN = counts_na) %>% data.frame

table(raw$Year.of.Birth.10, useNA = "always")



# ------------------------------------------------------------------------------
# check values for each variable
# ------------------------------------------------------------------------------
apply(raw[, grep("Role.", names(raw), fixed = TRUE)], MARGIN=2, FUN = table)

table(raw$Contract.Value.Band...see.note.A, useNA = "always")

table(raw$Grant.Category.Code, useNA = "always")

apply(raw[, grep("RFCD.Code", names(raw), fixed = TRUE)], MARGIN=2, FUN = table)

apply(raw[, grep("With.PHD.", names(raw), fixed = TRUE)], MARGIN=2, FUN = table)



# ------------------------------------------------------------------------------
# preprocess data
# ------------------------------------------------------------------------------

# In many cases, missing values in categorical data will be converted to a value of "Unk"
raw$Sponsor.Code <- as.character(raw$Sponsor.Code)
raw$Sponsor.Code[raw$Sponsor.Code == ""] <- "Unk"
raw$Sponsor.Code <- factor(paste("Sponsor", raw$Sponsor.Code, sep = ""))

raw$Grant.Category.Code <- as.character(raw$Grant.Category.Code)
raw$Grant.Category.Code[raw$Grant.Category.Code == ""] <- "Unk"
raw$Grant.Category.Code <- factor(paste("GrantCat", raw$Grant.Category.Code, sep = ""))

raw$Contract.Value.Band...see.note.A <- as.character(raw$Contract.Value.Band...see.note.A)
raw$Contract.Value.Band...see.note.A[raw$Contract.Value.Band...see.note.A == ""] <- "Unk"
raw$Contract.Value.Band...see.note.A <- factor(paste("ContractValueBand", raw$Contract.Value.Band...see.note.A, sep = ""))



# ----------
# Change missing Role.1 information to Unk
raw$Role.1 <- as.character(raw$Role.1)
raw$Role.1[raw$Role.1 == ""] <- "Unk"



# ----------
# Get the unique values of the birth years and department codes. These will be used later to make factor variables
bYears <- unique(do.call("c", raw[,grep("Year.of.Birth", names(raw), fixed = TRUE)]))
bYears <- bYears[!is.na(bYears)]

dpmt <- unique(do.call("c", raw[,grep("Dept.No", names(raw), fixed = TRUE)]))
dpmt <- sort(dpmt[!is.na(dpmt)])



# ----------
# At this point, the data for investigators is in different columns. We'll take this "horizontal" format and convert it to a
# "vertical" format where the data are stacked. This will make some of the data processing easier.

# Split up the data by role number (1-15) and add any missing columns (roles 1-5 have more columns than the others)

tmp <- vector(mode = "list", length = 15)

for(i in 1:15) {
  tmpData <- raw[, c("Grant.Application.ID", grep(paste("\\.", i, "$", sep = ""), names(raw), value = TRUE))]
  names(tmpData) <- gsub(paste("\\.", i, "$", sep = ""), "", names(tmpData))
  if(i == 1) nms <- names(tmpData)
  if(all(names(tmpData) != "RFCD.Code")) tmpData$RFCD.Code <- NA
  if(all(names(tmpData) != "RFCD.Percentage")) tmpData$RFCD.Percentage <- NA
  if(all(names(tmpData) != "SEO.Code")) tmpData$SEO.Code <- NA
  if(all(names(tmpData) != "SEO.Percentage")) tmpData$SEO.Percentage <- NA
  
  tmp[[i]] <- tmpData[,nms]
  rm(tmpData)
}


# Stack them up and remove any rows without role information
vertical <- do.call("rbind", tmp)
vertical <- subset(vertical, Role != "")

head(vertical)



# ----------
# Reformat some of the variables to make complete factors, correctly encode missing data or to make the factor levels more descriptive.

vertical$Role <- factor(as.character(vertical$Role))

vertical$Year.of.Birth <- factor(paste(vertical$Year.of.Birth), levels = paste(sort(bYears)))
vertical$Country.of.Birth <- gsub(" ", "", as.character(vertical$Country.of.Birth))
vertical$Country.of.Birth[vertical$Country.of.Birth == ""] <- NA
vertical$Country.of.Birth <- factor(vertical$Country.of.Birth)

vertical$Home.Language <- gsub("Other", "OtherLang", as.character(vertical$Home.Language))
vertical$Home.Language[vertical$Home.Language == ""] <- NA
vertical$Home.Language <- factor(vertical$Home.Language)

vertical$Dept.No. <- paste("Dept", vertical$Dept.No., sep = "")
vertical$Dept.No.[vertical$Dept.No. == "DeptNA"] <- NA
vertical$Dept.No. <- factor(vertical$Dept.No.)

vertical$Faculty.No. <- paste("Faculty", vertical$Faculty.No., sep = "")
vertical$Faculty.No.[vertical$Faculty.No. == "FacultyNA"] <- NA
vertical$Faculty.No. <- factor(vertical$Faculty.No.)

vertical$RFCD.Code <- paste("RFCD", vertical$RFCD.Code, sep = "")
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCDNA"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCDNA"] <- NA
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCD0"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCD0"] <- NA
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCD999999"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCD999999"] <- NA
vertical$RFCD.Code <- factor(vertical$RFCD.Code)

vertical$SEO.Code <- paste("SEO", vertical$SEO.Code, sep = "")
vertical$SEO.Percentage[vertical$SEO.Code == "SEONA"] <- NA
vertical$SEO.Code[vertical$SEO.Code == "SEONA"] <- NA
vertical$SEO.Percentage[vertical$SEO.Code == "SEO0"] <- NA
vertical$SEO.Code[vertical$SEO.Code  == "SEO0"] <- NA
vertical$SEO.Percentage[vertical$SEO.Code == "SEO999999"] <- NA
vertical$SEO.Code[vertical$SEO.Code== "SEO999999"] <- NA
vertical$SEO.Code <- factor(vertical$SEO.Code)

vertical$No..of.Years.in.Uni.at.Time.of.Grant <- as.character(vertical$No..of.Years.in.Uni.at.Time.of.Grant)
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ""] <- "DurationUnk"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">=0 to 5"] <- "Duration0to5"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">5 to 10"] <- "Duration5to10"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">10 to 15"] <- "Duration10to15"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == "more than 15"] <- "DurationGT15"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == "Less than 0"] <- "DurationLT0"
vertical$No..of.Years.in.Uni.at.Time.of.Grant <- factor(vertical$No..of.Years.in.Uni.at.Time.of.Grant)


names(vertical)



# ------------------------------------------------------------------------------
# Create variables
# ------------------------------------------------------------------------------
# A function to shorten the role titles

shortNames <- function(x, pre = ""){
  x <- gsub("EXT_CHIEF_INVESTIGATOR",  "ECI", x)
  x <- gsub("STUD_CHIEF_INVESTIGATOR", "SCI", x)
  x <- gsub("CHIEF_INVESTIGATOR",      "CI", x)
  x <- gsub("DELEGATED_RESEARCHER",    "DR", x)
  x <- gsub("EXTERNAL_ADVISOR",        "EA", x)
  x <- gsub("HONVISIT",                "HV", x)
  x <- gsub("PRINCIPAL_SUPERVISOR",    "PS", x)
  x <- gsub("STUDRES",                 "SR", x)
  x <- gsub("Unk",                     "UNK", x)
  other <- x[x != "Grant.Application.ID"]
  c("Grant.Application.ID", paste(pre, other, sep = ""))
}


# A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}



# ----------
# Calculate the total number of people identified on the grant

people <- ddply(vertical, .(Grant.Application.ID), function(x) c(numPeople = nrow(x)))
head(people)



# ----------
# Calculate the number of people per role

investCount <- ddply(vertical, .(Grant.Application.ID),
                     function(x) as.data.frame(t(as.matrix(table(x$Role)))),
                     .parallel = cores > 1)

# Clean up the names
names(investCount) <- shortNames(names(investCount), "Num")

head(investCount)



# ----------
# For each role, calculate the frequency of people in each age group
investDOB <- ddply(vertical, .(Grant.Application.ID),
                   function(x) {
                     tabDF <- as.data.frame(table(x$Role, x$Year.of.Birth))
                     out <- data.frame(t(tabDF$Freq))
                     names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                     out
                   },
                   .parallel = cores > 1)

names(investDOB) <- shortNames(names(investDOB))

investDOB <- noZV(investDOB)

head(investDOB)



# ----------
# For each role, calculate the frequency of people from each country

investCountry <- ddply(vertical, .(Grant.Application.ID),
                       function(x) {
                         tabDF <- as.data.frame(table(x$Role, x$Country.of.Birth))
                         out <- data.frame(t(tabDF$Freq))
                         names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                         out
                       },
                       .parallel = cores > 1)

names(investCountry) <- shortNames(names(investCountry))

investCountry <- noZV(investCountry)

head(investCountry)



# ----------
# For each role, calculate the frequency of people for each language

investLang <- ddply(vertical, .(Grant.Application.ID),
                    function(x) {
                      tabDF <- as.data.frame(table(x$Role, x$Home.Language))
                      out <- data.frame(t(tabDF$Freq))
                      names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                      out
                    },
                    .parallel = cores > 1)

names(investLang) <- shortNames(names(investLang))

investLang <- noZV(investLang)

head(investLang)



# ----------
# For each role, determine who as a Ph.D.

investPhD <- ddply(vertical, .(Grant.Application.ID),
                   function(x) {
                     tabDF <- as.data.frame(table(x$Role, x$With.PHD))
                     out <- data.frame(t(tabDF$Freq))
                     names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                     out
                   },
                   .parallel = cores > 1)

investPhD <- investPhD[,-grep("\\.$", names(investPhD))]

names(investPhD) <- shortNames(names(investPhD))

names(investPhD) <- gsub("Yes ", "PhD", names(investPhD))

investPhD <- noZV(investPhD)

head(investPhD)



# ----------
# For each role, calculate the number of successful and unsuccessful grants

investGrants <- ddply(vertical, .(Grant.Application.ID, Role),
                      function(x) {
                        data.frame(Success = sum(x$Number.of.Successful.Grant, na.rm = TRUE),
                                   Unsuccess = sum(x$Number.of.Unsuccessful.Grant, na.rm = TRUE))
                        
                      },
                      .parallel = cores > 1)

investGrants <- reshape(investGrants, direction = "wide", idvar = "Grant.Application.ID", timevar = "Role")

investGrants[is.na(investGrants)] <- 0

names(investGrants) <- shortNames(names(investGrants))

investGrants <- noZV(investGrants)

head(investGrants)



# ----------
# Create variables for each role/department combination

investDept <- ddply(vertical, .(Grant.Application.ID),
                    function(x) {
                      tabDF <- as.data.frame(table(x$Role, x$Dept.No.))
                      out <- data.frame(t(tabDF$Freq))
                      names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                      out
                    },
                    .parallel = cores > 1)

names(investDept) <- shortNames(names(investDept))

investDept <- noZV(investDept)

head(investDept)



# ----------
# Create variables for each role/faculty

investFaculty <- ddply(vertical, .(Grant.Application.ID),
                       function(x) {
                         tabDF <- as.data.frame(table(x$Role, x$Faculty.No.))
                         out <- data.frame(t(tabDF$Freq))
                         names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
                         out
                       },
                       .parallel = cores > 1)

names(investFaculty) <- shortNames(names(investFaculty))

investFaculty <- noZV(investFaculty)

head(investFaculty)



# ----------
# Create dummy variables for each tenure length

investDuration <- ddply(vertical, .(Grant.Application.ID),
                        function(x) as.data.frame(t(as.matrix(table(x$No..of.Years.in.Uni.at.Time.of.Grant)))),
                        .parallel = cores > 1)

investDuration[is.na(investDuration)] <- 0

head(investDuration)



# ----------
# Create variables for the number of publications per journal type. Note that we also compute the total number, which should be
# removed for models that cannot deal with such a linear dependency

totalPub <- ddply(vertical, .(Grant.Application.ID),
                  function(x) {
                    data.frame(AstarTotal = sum(x$A., na.rm = TRUE),
                               ATotal = sum(x$A, na.rm = TRUE),
                               BTotal = sum(x$B, na.rm = TRUE),
                               CTotal = sum(x$C, na.rm = TRUE),
                               allPub = sum(c(x$A., x$A, x$B, x$C), na.rm = TRUE))
                    
                  },
                  .parallel = cores > 1)

head(totalPub)



# ----------
# Create variables for the number of publications per journal type per role.

investPub <- ddply(vertical, .(Grant.Application.ID, Role),
                   function(x) {
                     data.frame(Astar = sum(x$A., na.rm = TRUE),
                                A = sum(x$A, na.rm = TRUE),
                                B = sum(x$B, na.rm = TRUE),
                                C = sum(x$C, na.rm = TRUE))
                     
                   },
                   .parallel = cores > 1)

investPub <- reshape(investPub, direction = "wide", idvar = "Grant.Application.ID", timevar = "Role")

investPub[is.na(investPub)] <- 0

names(investPub) <- shortNames(names(investPub))

investPub <- noZV(investPub)

head(investPub)



# ----------
# Create variables for each RFCD code

RFCDcount <- ddply(vertical, .(Grant.Application.ID),
                   function(x) as.data.frame(t(as.matrix(table(x$RFCD.Code)))),
                   .parallel = cores > 1)

RFCDcount <- noZV(RFCDcount)

head(RFCDcount)



# ----------
# Create variables for each SEO code

SEOcount <- ddply(vertical, .(Grant.Application.ID),
                  function(x) as.data.frame(t(as.matrix(table(x$SEO.Code)))),
                  .parallel = cores > 1)

SEOcount <- noZV(SEOcount)

head(SEOcount)



# ----------
# Make dummy vars out of grant-specific data

grantData <- raw[, c("Sponsor.Code", "Contract.Value.Band...see.note.A", "Grant.Category.Code")]


# Make a lubridate object for the time, then derive the day, week and month info
startTime <- dmy(raw$Start.date)

grantData$Month <- factor(as.character(month(startTime, label = TRUE)))
grantData$Weekday <- factor(as.character(wday(startTime, label = TRUE)))
grantData$Day <- day(startTime)
grantData$Yday <- yday(startTime)
grantYear <- year(startTime)



# ----------
# Use the dummyVars function to create binary variables for grant-specific variables

dummies <- dummyVars(~., data = grantData, levelsOnly = TRUE)
grantData <- as.data.frame(predict(dummies, grantData))
names(grantData) <- gsub(" ", "", names(grantData))

grantData$Grant.Application.ID <- raw$Grant.Application.ID
grantData$Class <- factor(ifelse(raw$Grant.Status, "successful", "unsuccessful"))
grantData$Grant.Application.ID <- raw$Grant.Application.ID

grantData$is2008 <- year(startTime) == 2008
grantData <- noZV(grantData)



# ----------
# Merge all the predictors together, remove zero variance columns and merge in the outcome data
summarized <- merge(investCount, investDOB)
summarized <- merge(summarized, investCountry)
summarized <- merge(summarized, investLang)
summarized <- merge(summarized, investPhD)
summarized <- merge(summarized, investGrants)
summarized <- merge(summarized, investDept)
summarized <- merge(summarized, investFaculty)
summarized <- merge(summarized, investDuration)
summarized <- merge(summarized, investPub)
summarized <- merge(summarized, totalPub)
summarized <- merge(summarized, people)
summarized <- merge(summarized, RFCDcount)
summarized <- merge(summarized, SEOcount)

summarized <- merge(summarized, grantData)


# Remove the ID column
summarized$Grant.Application.ID <- NULL
print(str(summarized))



# ------------------------------------------------------------------------------
# split training and test data
# ------------------------------------------------------------------------------
# We'll split all of the pre-2008 data into the training set and a portion of the 2008 data too

training <- subset(summarized, !is2008)
pre2008 <- 1:nrow(training)
year2008 <- subset(summarized, is2008)



# ----------
# Now randomly select some 2008 data for model training and add it back into the existing training data
set.seed(568)
inTrain <- createDataPartition(year2008$Class, p = 3/4)[[1]]

training2 <- year2008[ inTrain,]
testing   <- year2008[-inTrain,]
training <- rbind(training, training2)

training$is2008 <- testing$is2008 <- NULL

training <- noZV(training)
testing <- testing[, names(training)]



# ------------------------------------------------------------------------------
# Create variable for factor predictos
# ------------------------------------------------------------------------------
# In the classification tree, there is a different set of predictors that use factor encodings of some of the predictors

factorPredictors <- names(training)[names(training) != "Class"]
factorPredictors <- factorPredictors[!grepl("Sponsor[0-9]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("SponsorUnk", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("ContractValueBand[A-Z]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("GrantCat", factorPredictors)]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Month))]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Weekday))]

factorForm <- paste("Class ~ ", paste(factorPredictors, collapse = "+"))
factorForm <- as.formula(factorForm)



# ------------------------------------------------------------------------------
# Create variable for fullSet
# ------------------------------------------------------------------------------
# Create two character vectors for different predictor sets. One will have all the predictors (called 'fullSet').
# Another has some of the sparse predictors removed for models that require such filtering. This will be called 'reducedSet'
# (predictors without sparse or Near Zero Variance predictors). This set will also have predictors removed that are almost completely
# correlated with other predictors

fullSet <- names(training)[names(training) != "Class"]



# ------------------------------------------------------------------------------
# Create variable for reduced set
# ------------------------------------------------------------------------------
# Some are extremely correlated, so remove
predCorr <- cor(training[,fullSet])
highCorr <- findCorrelation(predCorr, .99)
fullSet <- fullSet[-highCorr]

isNZV <- nearZeroVar(training[,fullSet], saveMetrics = TRUE, freqCut = floor(nrow(training)/5))
fullSet <-  rownames(subset(isNZV, !nzv))
str(fullSet)

reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(training)/50)))



# ----------
# Perfectly collinear predictors (due to their construction) March and Sunday were selected because they have the lowest frequency of all months and days
reducedSet <- reducedSet[(reducedSet != "allPub") &
                           (reducedSet != "numPeople") &
                           (reducedSet != "Mar") &
                           (reducedSet != "Sun")
                         ]

# all months and days
reducedSet <- reducedSet[(reducedSet != "allPub") &
                           (reducedSet != "numPeople") &
                           (reducedSet != "Mar") &
                           (reducedSet != "Sun")
                         ]
str(reducedSet)



# ----------
length(fullSet)
length(reducedSet)



# ------------------------------------------------------------------------------
# Save data
# ------------------------------------------------------------------------------
save(training, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/training")
save(testing, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/testing")
save(fullSet, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/fullSet")
save(reducedSet, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/reducedSet")
save(pre2008, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/pre2008")
