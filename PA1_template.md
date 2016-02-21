# Reproducible Research: Peer Assessment 1
## Preparation
Clean environment

```r
rm(list=ls())
```
Load necessary packages 

```r
library(lubridate)
library(ggplot2)
library(dplyr)
```
Set working directory

```r
setwd("C:/Data Science/5. Reproducible Research/Assignment1/RepData_PeerAssessment1")
```
Read data into memory

```r
stepData <- read.csv("activity.csv")
```

## Analysis
###Total number of steps taken per day
The numbers are summarized in a new dataframe called "dailySteps" produced by the code below.

```r
dailySteps <- summarise(group_by(stepData, date), 
                        "totalsteps" = sum(steps, na.rm=TRUE))
```

###Histogram of the total number of steps taken each day
Using the dataframe produced above, the following code is applied to make the requested histogram.

```r
print(qplot(dailySteps$totalsteps, geom="histogram", 
      xlab = "total steps per day", 
      ylab = "count of days when it happened",
      main = "Histogram of total steps per day, n = 61"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

###Mean and median of the total number of steps taken per day
For this calculation still the "dailySteps" dataframe is used. The mean and the median are slightly different. 

```r
print(paste("mean:", round(mean(dailySteps$totalsteps), digits=0)))
```

```
## [1] "mean: 9354"
```

```r
print(paste("median:", median(dailySteps$totalsteps)))
```

```
## [1] "median: 10395"
```

###Time series plot of the 5-minute interval and the average number of steps taken
The steps are averaged across all days.
First a new dataframe is produced called "intervalSteps" then the plot is made.

```r
intervalSteps <- summarise(group_by(stepData, interval), 
                           "average steps in interval" = mean(steps, na.rm=TRUE))
print(qplot(intervalSteps$interval, intervalSteps$`average steps in interval`, data=intervalSteps, geom="line",
            xlab = "hours-minutes of the day", 
            ylab = "average number of steps per 5-minute interval",
            main = "Time series of the 5-minute interval and the average number of steps taken"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

###The 5-minute interval which contains the maximum number of steps
The most active time interval is shown to be around 17 o'clock in the afternoon.

```r
intervalSteps$interval[max(intervalSteps$`average steps in interval`)]
```

```
## [1] 1705
```

###The total number of missing values
The dataset contains some significant number of NAs shown by the code below.

```r
length(stepData$steps[is.na(stepData$steps)])
```

```
## [1] 2304
```

###New dataset with the missing data filled in
For filling in all of the missing values in the dataset the "intervalSteps" dataframe is used which contains the means for all 5-minute intervals excluding NA-s. These means replace missing values in the modified dataframe called "stepDataMod".

```r
stepDataMod <- left_join(stepData, intervalSteps, by=rownames(interval))
stepDataMod$steps[is.na(stepDataMod$steps)] <- stepDataMod$`average steps in interval`
```

###Histogram of the total number of steps taken each day and the mean and median in the new dataset with the missing data filled in
These values differ from the estimates from the original dataset.

```r
dailyStepsMod <- summarise(group_by(stepDataMod, date), 
                        "totalsteps" = sum(steps, na.rm=TRUE))

print(qplot(dailyStepsMod$totalsteps, geom="histogram", 
xlab = "total steps per day", 
ylab = "count of days when it happened",
main = "Modified histogram of total steps per day, n = 61"))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
print(paste("modified mean:", round(mean(dailyStepsMod$totalsteps), digits=0)))
```

```
## [1] "modified mean: 10766"
```

```r
print(paste("modified median:", round(median(dailyStepsMod$totalsteps), digits=0)))
```

```
## [1] "modified median: 10766"
```

###Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
Two new columns are added to the dataset: the name of the days and the factor variable.

```r
stepData$day <- weekdays(as.Date(stepData$date))
for (i in 1:length(stepData$day)){
if (stepData$day[i] %in% c("Saturday","Sunday")) {stepData$daytype[i] <- "weekend"} 
  else {stepData$daytype[i] <- "weekday"}
}
stepData$daytype <- as.factor(stepData$daytype)
```

###Time series plot of the 5-minute interval and the average number of steps taken in weekday days or weekend days
The code below produces the requested plots.

```r
daytypeIntervalSteps <- summarise(group_by(stepData, daytype, interval), 
                                  "average steps in interval" = mean(steps, na.rm=TRUE))
print(qplot(daytypeIntervalSteps$interval, daytypeIntervalSteps$`average steps in interval`, 
            data=daytypeIntervalSteps, facets=daytype~., geom="line",
            xlab = "hours-minutes of the day", 
            ylab = "average number of steps per 5-minute interval",
            main = "Time series of the 5-minute interval and the average number of steps taken"))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
