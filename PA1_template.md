---
title: "PA1_template.Rmd"
author: "Ed Spinella"
date: "Sunday, July 15, 2014"
output: html_document
---
###Peer Assessment 1 - Reproducible Research

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

####Data

Data
The data for this assignment can be downloaded from the course web site at the following location:
                 https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:

- steps: Number of steps taking in a five-minute interval (missing values are coded as 'NA')

-	date: The date on which the measurement was taken in YYYY-MM-DD format

-	interval: Identifier for the five-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file format and contains a total of 17,568 observations.

####Loading and preprocessing the data
Show any code that is needed to

- Load the data (i.e. read.csv())

- Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(lattice)
library(knitr)
library(plyr)
library(ggplot2)

setwd("/Users/Ed/Desktop/EdData/Coursera/ReproducibleResearch/Assignment1")
data <- read.csv(unz("activity.zip", "activity.csv"), colClasses = c("numeric", "Date", "numeric"))
```

#### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

- Make a histogram of the total number of steps taken each day

- Calculate and report the mean and median total number of steps taken per day



```r
total.steps.by.date <- aggregate(steps ~ date, data= data, FUN=sum)

hist(total.steps.by.date$steps, main="Number of Steps", xlab="Total number of steps taken each day", col="red", breaks = 5)  
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(total.steps.by.date$steps)
```

```
## [1] 10766
```

```r
median(total.steps.by.date$steps)
```

```
## [1] 10765
```
The mean number of steps taken per day is 10766 and the median total number of steps taken per day is 10765. 


#### What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 

```r
steps.by.interval <- aggregate(steps ~ interval, data = data, FUN=mean)
plot(steps.by.interval, type="l",xlab="5-minute interval",  ylab="Average number of steps taken",main="Average daily step activity pattern", xaxt = "n") 
axis(side = 1, labels.at <- seq(0, 2000, 500), labels = c("00:00", "05:00", "10:00", "15:00", "20:00")) 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
interval.with.max.num.steps <- steps.by.interval[which.max(steps.by.interval$steps), ]
interval.with.max.num.steps
```

```
##     interval steps
## 104      835 206.2
```

The time interval 0830AM to 0840AM contains the maximum average number of steps at 206.2.

####Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dim(data[is.na(data$steps), ])[1] 
```

```
## [1] 2304
```
The total number of missing values is 2304.


```r
merged.activity <- merge(data, steps.by.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(merged.activity$steps)
merged.activity$steps[nas] <- merged.activity$steps.y[nas]
merged.activity <- merged.activity[,c(1:3)]
```
The above code removes the N/A's from the variable "steps" in the the orginal dataset and replaces with the averaged variable "steps" values from the steps.by.interval dataset. The new dataset, merged.activity is equal to the original dataset but with the missing data filled in.


```r
total.daily.steps <- tapply(merged.activity$steps, merged.activity$date, FUN = sum)
hist(total.daily.steps, main="Number of Steps", xlab="Total number of steps taken each day", col="red", breaks = 5)   
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
mean(total.daily.steps)
```

```
## [1] 10766
```

```r
median(total.daily.steps)
```

```
## [1] 10766
```
The impact of imputing missing data on the estimates of the total daily number of steps is negligible.
Mean 10766|median 10765 from the original dataset compares closely to the mean 10766|median 10766 from the total.daily.steps dataset  
 


 
