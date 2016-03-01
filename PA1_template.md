---
title: "PA1_template.rmd"
author: "Stijn"
date: "Monday, January 31, 2016, modified on 02/18/2016"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

# Loading and preprocessing the data, show the structure

```r
activityData <- read.csv('activity.csv')
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


# Q1: What is the mean total number of steps taken per day?

```r
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

## Make a histogram of the total number of steps taken per day

```r
hist(stepsByDay, 10, main = "Histogram of total number of steps taken per day", col = "red", xlab = "Total Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

## Calculate and report the mean and median total number of steps taken per day

```r
stepsByDayMean <- mean(stepsByDay)
```
## [1] 9354


```r
stepsByDayMedian <- median(stepsByDay)
```
## [1] 10395


# Q2: What is the average daily activity pattern?
We will remove the NA data first


```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
activityData_rm<-activityData[which(!is.na(activityData$steps)),]
```

##Compute the average daily activity pattern for each 5 minute interval. There are 288 5-minute intervals per day (==24*12)


```r
dailyActivity<-tapply(activityData_rm$steps, activityData_rm$interval, mean)
plot(y = dailyActivity, x = names(dailyActivity), type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


##Find the interval with the most steps during the day

```r
dailyActivity[dailyActivity==max(dailyActivity)]
```

```
##      835 
## 206.1698
```

## Imputing missing values


```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

We will use the mean of the 5-minute interval. We will now create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityData_new <- activityData
activityData_new[which(is.na(activityData_new$steps)),1]<-
        dailyActivity[as.character(activityData_new[which(is.na(activityData_new$steps)),3])]
```


```r
sum(is.na(activityData_new$steps))
```

```
## [1] 0
```


```r
stepsByDay_new<-tapply(activityData_new$steps, activityData_new$date, sum)
```

Make the histogram we made in the first part of the analysis, in order to visually see if there is a big effect.

```r
hist(stepsByDay_new, 10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", col = "blue", xlab = "Steps",
     ylim =c(0, 25))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 


```r
mean(stepsByDay_new)
```

```
## [1] 10766.19
```

## [1] 10766


```r
median(stepsByDay_new)
```

```
## [1] 10766.19
```

## [1] 10766

Imputing missing data hardly makes a difference

```r
mean(stepsByDay_new)-mean(stepsByDay)
```

```
## [1] 1411.959
```


```r
median(stepsByDay_new)-median(stepsByDay)
```

```
## [1] 371.1887
```

# Q3: Are there differences in activity patterns between weekdays and weekends?
We will create a factor variable in the dataset with two levels called “weekday” and “weekend” to indicate whether a given date is a weekday or weekend day. We use the weekdays() function to map dates to names of the week, and infer weekend or weekday.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activityData_new$daytype <- as.factor(sapply(activityData_new$date, daytype))
```


##Make the plot

```r
par(mfrow = c(2, 1))

steps.weekend <- aggregate(steps ~ interval, data = activityData_new, subset = activityData_new$daytype == "weekend", FUN = mean)
plot(steps.weekend, type = "l", main = "Weekend")
    
steps.weekday <- aggregate(steps ~ interval, data = activityData_new, subset = activityData_new$daytype == "weekday", FUN = mean)
plot(steps.weekday, type = "l", main = "Weekday")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 
