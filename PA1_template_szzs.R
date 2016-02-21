rm(list=ls())

library(lubridate)
library(ggplot2)
library(dplyr)

setwd("C:/Data Science/5. Reproducible Research/Assignment1/RepData_PeerAssessment1")

#Read data into memory

stepData <- read.csv("activity.csv")

#Prepare data for analysis by 1. adding zeros to "interval" until all have 4 characters, 2. pasting 
#"date" and "interval" together and 3. formatting the date and time information. 

#stepData$datetime <- parse_date_time(paste(stepData$date, sprintf("%04d", stepData$interval)), "y-m-d Hm")

#Without missing values
#1.	Calculate the total number of steps taken per day

dailySteps <- summarise(group_by(stepData, date), 
                        "totalsteps" = sum(steps, na.rm=TRUE))

#2.	If you do not understand the difference between a histogram and a barplot, research the difference 
#between them. Make a histogram of the total number of steps taken each day

print(qplot(dailySteps$totalsteps, geom="histogram", 
      xlab = "total steps per day", 
      ylab = "count of days when it happened",
      main = "Histogram of total steps per day, n = 61"))

#3.	Calculate and report the mean and median of the total number of steps taken per day

print(paste("mean:", round(mean(dailySteps$totalsteps), digits=0)))
print(paste("median:", median(dailySteps$totalsteps)))

#4.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number 
#of steps taken, averaged across all days (y-axis)

intervalSteps <- summarise(group_by(stepData, interval), 
                           "average steps in interval" = mean(steps, na.rm=TRUE))
print(qplot(intervalSteps$interval, intervalSteps$`average steps in interval`, data=intervalSteps, geom="line",
            xlab = "hours-minutes of the day", 
            ylab = "average number of steps per 5-minute interval",
            main = "Time series of the 5-minute interval and the average number of steps taken"))

#5.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number 
#of steps?

intervalSteps$interval[max(intervalSteps$`average steps in interval`)]

#With missing values
#1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of 
#rows with NAs)

length(stepData$steps[is.na(stepData$steps)])

#2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not 
#need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 
#5-minute interval, etc.

#I use intervalSteps containing the mean for all 5-minute intervals exclusing NA-s 

#3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.

stepDataMod <- left_join(stepData, intervalSteps, by=rownames(interval))
stepDataMod$steps[is.na(stepDataMod$steps)] <- stepDataMod$`average steps in interval`

#4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
#median total number of steps taken per day. Do these values differ from the estimates from the first 
#part of the assignment? What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?

dailyStepsMod <- summarise(group_by(stepDataMod, date), 
                        "totalsteps" = sum(steps, na.rm=TRUE))

print(qplot(dailyStepsMod$totalsteps, geom="histogram", 
xlab = "total steps per day", 
ylab = "count of days when it happened",
main = "Modified histogram of total steps per day, n = 61"))

print(paste("modified mean:", round(mean(dailyStepsMod$totalsteps), digits=0)))
print(paste("modified median:", round(median(dailyStepsMod$totalsteps), digits=0)))

#5.	Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
#whether a given date is a weekday or weekend day.

stepData$day <- weekdays(as.Date(stepData$date))
for (i in 1:length(stepData$day)){
if (stepData$day[i] %in% c("Saturday","Sunday")) {stepData$daytype[i] <- "weekend"} 
  else {stepData$daytype[i] <- "weekday"}
}
stepData$daytype <- as.factor(stepData$daytype)

#6.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See 
#the README file in the GitHub repository to see an example of what this plot should look like using 
#simulated data.

daytypeIntervalSteps <- summarise(group_by(stepData, daytype, interval), 
                                  "average steps in interval" = mean(steps, na.rm=TRUE))
print(qplot(daytypeIntervalSteps$interval, daytypeIntervalSteps$`average steps in interval`, 
            data=daytypeIntervalSteps, facets=daytype~., geom="line",
            xlab = "hours-minutes of the day", 
            ylab = "average number of steps per 5-minute interval",
            main = "Time series of the 5-minute interval and the average number of steps taken"))
