---
title: "Reproducible_PA1"
author: "Varunesh"
date: "Sunday, March 15, 2015"
output: html_document
fig_caption: yes
---

Reproducible Research: Peer Assessment 1
==========================================
Created by Varunesh Rai on March 15, 2015

### Basic settings

```r
echo = TRUE  
options(scipen = 1)  
```

### Loading and processing the data

```r
data <- read.csv("C:/Users/Arunesh Rai/Documents/activity.csv", header=T, quote="\"",colClasses= c("numeric", "Date", "factor"))
data_noNA<-na.omit(data)
library(ggplot2)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
g1<-aggregate(data_noNA$steps~data_noNA$date,data=data_noNA,FUN=sum)
names(g1)<-c("date","steps")
ggplot(g1, aes(date, steps)) + geom_histogram(stat = "identity", colour = "red", fill = "red", width = 0.7) + labs(title = "Histogram of Total Number of Steps Taken Each Day(No missing values)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
mean(g1$steps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(g1$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
g2<-aggregate(data_noNA$steps~as.numeric(as.character(data_noNA$interval)),data=data_noNA,FUN=mean)
names(g2)<-c("interval","steps")
plot(g2$interval, g2$steps, type="l",ylab = "No. of steps", xlab = "5-minute interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
g2[g2$steps==max(g2$steps),]$interval
```

```
## [1] 835
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval(calculated in data frame g2) to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_na<-data
for(i in 1:nrow(data_na))
{
  if(is.na(data_na$steps[i]))
  {
	data_na$steps[i]<-g2[which(data_na$interval[i]==g2$interval),]$steps
	}
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
g3<-aggregate(data_na$steps~data_na$date,data=data_na,FUN=sum)
names(g3)<-c("date","steps")
ggplot(g3, aes(date, steps)) + geom_histogram(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + labs(title = "Histogram of Total Number of Steps Taken Each Day(No missing values)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newmean<-mean(g3$steps)
newmean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newmedian<-median(g3$steps)
newmedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(g1$steps)
oldMedian <- median(g1$steps)
newmean - oldMean
```

```
## [1] 0
```

```r
newmedian - oldMedian
```

```
## [1] 1.188679
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data_na$weekdays <- factor(format(data_na$date, "%A"))
levels(data_na$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(data_na$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(data_na$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(data_na$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(data_na$steps, 
                      list(interval = as.numeric(as.character(data_na$interval)), 
                           weekdays = data_na$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
