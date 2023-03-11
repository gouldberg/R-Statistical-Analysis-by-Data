
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\classroom")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SII Project, classroom
# ------------------------------------------------------------------------------


data(classroom, package = "WWGbook")


str(classroom)


car::some(classroom)




# ----------
# convert to factor variable

SIIdata <- within(classroom,
                  {
                    sex <- factor(sex, levels = c(0,1), labels = c("M", "F"))
                    minority <- factor(minority, labels = c("Mnrt:No", "Mnrt:Yes"))
                    schoolid <- factor(schoolid)
                    classid <- factor(classid)
                    childid <- factor(childid)
                  })


str(SIIdata)




# ------------------------------------------------------------------------------
# Investigation of data hierarchy
# ------------------------------------------------------------------------------


dtID <- subset(SIIdata, select = c(schoolid, classid, childid))


names(dtID)



# ----------
any(duplicated(dtID))




# ----------

library(nlme)


# nlme::gsummary() provides a summary of variables, contained in a data frame,
# by groups of rows.
# In particular, the function can be used to determine whether there are variables that are invariant
# within the groups

# inv = TRUE:  only those variables, which are invariant within each group, are to be summarized.


head(gsummary(dtID, form = ~ childid, inv = TRUE))


head(gsummary(dtID, form = ~ classid, inv = TRUE))


head(gsummary(dtID, form = ~ schoolid, inv = TRUE))





# ------------------------------------------------------------------------------
# Identification of school-, class-, and pupil-level variables in the data frame
# ------------------------------------------------------------------------------


# school-level variables
( nms1 <- names(gsummary(SIIdata, form = ~ schoolid, inv = TRUE)) )




# ----------
# class-level variables
( nms2a <- names(gsummary(SIIdata, form = ~ classid, inv = TRUE)) )


# classid-specific
( nms2 <- nms2a[-match(nms1, nms2a)] )




# ----------
# pupil-level variables
( nms3a <- names(gsummary(SIIdata, form = ~ childid, inv = TRUE)) )


# childid-specific
( nms3a <- nms3a[-match(c(nms1, nms2), nms3a)] )



