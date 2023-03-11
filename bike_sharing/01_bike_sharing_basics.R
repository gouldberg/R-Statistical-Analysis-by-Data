setwd("//media//kswada//MyFiles//R//bike_sharing")

packages <- c("dplyr", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bike sharing
#   - contains daily counts of rented bicycles from the bicycle rental company Capital-Bikeshare in Washington D.C., 
#     along with weather and seasonal information.
#   - The data was kindly made openly available by Capital-Bikeshare.
#     Fanaee-T and Gama (2013) added weather data and season information.
#     The goal is to predict how many bikes will be rented depending on the weather and the day.
#     The data can be downloaded from the UCI Machine Learning Repository.
#
#   - New features were added to the dataset and not all original features were used for the examples here.
#     Here is the list of features that were used:
#        - Count of bicycles including both casual and registered users. The count is used as the target in the regression task.
#        - The season, either spring, summer, fall or winter.
#        - Indicator whether the day was a holiday or not.
#        - The year, either 2011 or 2012.
#        - Number of days since the 01.01.2011 (the first day in the dataset). This feature was introduced to take account of the trend over time.
#        - Indicator whether the day was a working day or weekend.
#        - The weather situation on that day. One of: clear, few clouds, partly cloudy, cloudy
#        - mist + clouds, mist + broken clouds, mist + few clouds, mist
#        - light snow, light rain + thunderstorm + scattered clouds, light rain + scattered clouds
#        - heavy rain + ice pallets + thunderstorm + mist, snow + mist
#        - Temperature in degrees Celsius.
#        - Relative humidity in percent (0 to 100).
#        - Wind speed in km per hour.
# ------------------------------------------------------------------------------

get.bike.data = function(data_dir){
  bike = read.csv(sprintf('%s//day.csv', data_dir), stringsAsFactors = FALSE)
  # See http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
  
  bike$weekday = factor(bike$weekday, levels=0:6, labels = c('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT'))
  bike$holiday = factor(bike$holiday, levels = c(0,1), labels = c('NO HOLIDAY', 'HOLIDAY'))
  bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c('NO WORKING DAY', 'WORKING DAY'))
  bike$season = factor(bike$season, levels = 1:4, labels = c('SPRING', 'SUMMER', 'FALL', 'WINTER'))
  bike$weathersit = factor(bike$weathersit, levels = 1:3, labels = c('GOOD', 'MISTY', 'RAIN/SNOW/STORM'))
  bike$mnth = factor(bike$mnth, levels = 1:12, labels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OKT', 'NOV', 'DEZ'))
  bike$yr[bike$yr == 0] = 2011
  bike$yr[bike$yr == 1] = 2012
  bike$yr = factor(bike$yr)
  bike$days_since_2011 = day_diff(bike$dteday, min(as.Date(bike$dteday)))
  
  # denormalize weather features:
  # temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
  bike$temp = bike$temp * (39 - (-8)) + (-8)
  # atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
  bike$atemp = bike$atemp * (50 - (16)) + (16)
  
  #windspeed: Normalized wind speed. The values are divided to 67 (max)
  bike$windspeed = 67 * bike$windspeed
  #hum: Normalized humidity. The values are divided to 100 (max)
  bike$hum = 100 * bike$hum
  
  
  dplyr::select(bike, -instant, -dteday, -registered, -casual, -atemp)
}



# ----------
# get.bike.task <- function(data_dir){
#  mlr::makeRegrTask(id='bike', data=get.bike.data(data_dir), target = 'cnt')
# }


# ----------
year_diff = function(date1, date2){
  day_diff(date1, date2) / 365.25
}

day_diff = function(date1, date2){
  as.numeric(difftime(as.Date(date1), as.Date(date2), units = 'days'))
}


# ----------
data_dir <- "//media//kswada//MyFiles//data//bike_sharing//"

bike <- get.bike.data(data_dir)

str(bike)

car::some(bike)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# cnt by season
histogram(~ cnt | season, data = bike)


# cnt by season + yr
histogram(~ cnt | season + yr, data = bike)


# cnt by season and wetherit
histogram(~ cnt | season + weathersit, data = bike)



# ----------
# cnt and temp
ggplot(data = bike, mapping = aes(x = temp, y = cnt)) + 
  geom_point() + geom_smooth() + facet_wrap(~ season, nrow=2)


# cnt and hum
ggplot(data = bike, mapping = aes(x = hum, y = cnt)) + 
  geom_point() + geom_smooth() + facet_wrap(~ season, nrow=2)


# cnt and windspeed
ggplot(data = bike, mapping = aes(x = windspeed, y = cnt)) + 
  geom_point() + geom_smooth() + facet_wrap(~ season, nrow=2)

ggplot(data = bike, mapping = aes(x = windspeed, y = cnt)) + 
  geom_point() + geom_smooth()


# ----------
# cnt and days_since_2011
ggplot(data = bike, mapping = aes(x = days_since_2011, y = cnt)) + 
  geom_point() + geom_smooth()



# ----------
Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}

MyVar2 <- c("temp", "hum", "windspeed", "days_since_2011")

Myxyplot(bike, MyVar2, "cnt")


