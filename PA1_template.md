

```r
title: 'Coursera: Reproducible Research - Project 1'
```

```
## Warning: NAs introduced by coercion
```

```
## Error in title:"Coursera: Reproducible Research - Project 1": NA/NaN argument
```

```r
author: "Kate Stone"
```

```
## Error in eval(expr, envir, enclos): object 'author' not found
```

```r
date: "December 5, 2018"
```

```
## Warning: NAs introduced by coercion
```

```
## Error in date:"December 5, 2018": NA/NaN argument
```

```r
output: html_document
```

```
## Error in eval(expr, envir, enclos): object 'output' not found
```

```r
##Loading and preprocessing the data
```

```r
activity <- read.csv("activity.csv")
```

##1. What is mean total number of steps taken per day?

###1.1. Calculate total number of steps taken per day


```r
StepsPerDay <- tapply(activity$steps, activity$date, sum)
```

###1.2. Make a histogram of the total number of steps taken each day


```r
hist(StepsPerDay, xlab = "Number of steps", main = "Histogram: Steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

###1.3. Calculate and report the mean and median of the total number of steps taken per day


```r
Mean <- mean(StepsPerDay, na.rm = TRUE)
Median <- median(StepsPerDay, na.rm = TRUE)
```

The mean and median number of steps per day are 1.0766189 &times; 10<sup>4</sup> and 10765, respectively.

##2. What is the average daily activity pattern?

###2.1 Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)), StepsPerInterval, xlab = "Interval", ylab = "Steps", main = "Average daily activity pattern", type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

###2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- names(StepsPerInterval[which.max(StepsPerInterval)])
maxSteps <- StepsPerInterval[which.max(StepsPerInterval)]
```

The 835 interval contains the maximum number of steps 206.1698113.

##3. Imputing missing values

###3.1. Calculate and report the total number of missing values in the dataset


```r
na <- sum(is.na(activity$steps))
```

The total number of missing values in the dataset is 2304.

###3.2. Devise a strategy for filling in all of the missing values in the dataset


```r
imputed_steps <- StepsPerInterval[match(activity$interval, names(StepsPerInterval))]
```
###3.3. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "steps")
```

###3.4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
StepsPerDay2 <- tapply(activity_imputed$steps, activity_imputed$date, sum)
hist(StepsPerDay2, xlab = "Number of steps", main = "Histogram: Steps per day (Imputed data)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
mean2 <- mean(StepsPerDay2, na.rm = TRUE)
median2 <- median(StepsPerDay2, na.rm = TRUE)
```

The mean and median number of steps per day including imputed data are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup>, respectively. The mean remains the same as prior to imputation, while the median value increased slightly.

##4. Are there differences in activity patterns between weekdays and weekends?

###4.1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_imputed$day <- ifelse(weekdays(as.Date(activity_imputed$date)) == "Saturday" | weekdays(as.Date(activity_imputed$date)) == "Sunday", "weekend", "weekday")
```

###4.2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
activity_by_date <- aggregate(steps~interval+day,data=activity_imputed,FUN=mean,na.action=na.omit)

library(ggplot2)

plot <- ggplot(activity_by_date, aes(interval, steps, color = day)) + 
        geom_line() +
        labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~day, ncol = 1, nrow=2)
print(plot)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
```

