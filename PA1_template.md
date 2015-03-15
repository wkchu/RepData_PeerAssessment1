---
title: "Reproducible Research Assignment 1"
output: html_document
---

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data ##

The following libraries are required for this work.


```r
require(dplyr)
require(ggplot2)
```

* The Activity moitoring data (Activity.csv) are loaded first.


```r
if (! exists("Activity")) {
    # Activity data
    file <- "./data/activity.csv"
    if (! file.exists(file)) {
        stop("Stop! File", file, "is missing.")
    }
    Activity <- read.csv(file) # 17568 obs. of 3 variables
}
```

## What is mean total number of steps taken per day? ##

The missing values are ignored in this assignment.

* First we calculate the total number of steps taken per day


```r
Activity1 = group_by(Activity, date)
Activity1.Daily = summarize(Activity1, sum(steps, na.rm=TRUE))
names(Activity1.Daily) = c("date", "steps")
```
and make a histgram of the total number of steps taken each day. Also calculate the mean and median of the total number of steps taken per day.

```r
with(Activity1.Daily, hist(steps))
```

![plot of chunk assignment_1_plot](figure/assignment_1_plot-1.png) 

```r
Activity1.Daily.mean = mean(Activity1.Daily$steps)
Activity1.Daily.median = median(Activity1.Daily$steps)
```
* The mean of the total number of steps taken per day is 9354 and the median is 10395.

## What is the average daily activity pattern? ##

* First we need to calculate the the average number of steps taken, averaged across all days.


```r
Activity2 = filter(Activity, ! is.na(steps))
Activity2.Intvl = mutate(Activity2, intvl = factor(interval))
Activity2.Intvl = group_by(Activity2.Intvl, intvl)
Activity2.Intvl = mutate(Activity2.Intvl, avg = mean(steps, na.rm=TRUE))

# Take the first day's avg values as our data set for plotting, since the avg values
# are repeated for each day.
num.interval = length(unique(Activity2.Intvl$interval))
times = strptime(sprintf("%04d", as.numeric(Activity2.Intvl[1:num.interval,]$interval)), 
                 format="%H%M")
avg_steps = Activity2.Intvl$avg[1:num.interval]
df2 = data.frame(times, avg_steps, ids = Activity2.Intvl$interval[1:num.interval])
```
* Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
plot(df2$times, df2$avg_steps, type="l")
```

![plot of chunk assignment_2_plot](figure/assignment_2_plot-1.png) 
* Then we find the 5-minute interval that contains the maximum number of steps.


```r
max_interval = df2$ids[which.max(df2$steps)]
```
The interval  contains the maximum number of steps on average across all the days in the dataset.

## Imputing missing values ##
* Calculate and report the total number of missing values in the dataset.

```r
num.na = length(filter(Activity, is.na(steps))$interval)
```
There are 2304 missing values (total number of rows with NA) in the data set.

* Strategy : We use the 'mean' for the 5-minute interval for filling in the missing steps values, and create a new data set.


```r
Activity3 = mutate(Activity, intvl = factor(interval))
Activity3 = group_by(Activity3, intvl)
Activity3 = mutate(Activity3, avg = mean(steps, na.rm=TRUE))
# Replace the missing values by the mean values of that interval.
steps.new = Activity3$steps
for (i in 1:length(steps.new)) {
    if (is.na(steps.new[i])) {
        steps.new[i] = Activity3$avg[i]
    }
}
# Replace the original $steps column by the new one with filling mean values
Activity3$steps = steps.new

Activity3 = group_by(Activity3, date)
Activity3.Daily = summarize(Activity3, sum(steps))
names(Activity3.Daily) = c("date", "steps")
```

* Make a histgram of the total number of steps taken each day. Also calculate the mean and median of the total number of steps taken per day.

```r
with(Activity3.Daily, hist(steps))
```

![plot of chunk assignment_3_plot](figure/assignment_3_plot-1.png) 

```r
Activity3.Daily.mean = mean(Activity3.Daily$steps)
Activity3.Daily.median = median(Activity3.Daily$steps)
```
The mean of the total number of steps taken per day is 10766 and the median is 10766.

* Do these values differ from the estimates from the first part of the assignment? 
From the result, yes. The *mean* was increased from 9354 to 10766 and the *median* was increased from 10395 to 10766.

* What is the impact of imputing missing data on the estimates of the total daily number of steps?
The frequency on the *0-to-5000* block was *reduced* and the *10000-to-15000* block (where the *mean* value falls) was *increased*, due to the the NA-filled-by-mean.

## Are there differences in activity patterns between weekdays and weekends? ##

* First create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. Based on the filed-in missing values data set.


```r
Activity4 = mutate(Activity3, weekday = factor(!(weekdays(as.Date(as.character(date))) 
                                          %in% c('Saturday','Sunday')), levels = c(T, F),
                                          labels = c("weekday", "weekend")))

Activity4 = group_by(Activity4, weekday, intvl)
Activity4 = mutate(Activity4, avg = mean(steps))
```
* Then we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
# number of intervals per day
num.interval = length(unique(Activity4$interval)) 
weekday.list = which(Activity4$weekday == "weekday")
weekend.list = which(Activity4$weekday == "weekend")
samples = c(weekday.list[1]:(weekday.list[1]+num.interval), 
            weekend.list[1]:(weekend.list[1]+num.interval))

df4 = with(Activity4[samples,], data.frame(interval, avg, weekday))
```
* Plot with ggplot system.

```r
gp <- ggplot(df4, aes(interval, avg))
gp + geom_line(color="blue", alpha=0.75) +
    facet_wrap( ~ weekday, nrow = 2) +
    labs(x="Interval", y="Average number of steps", title="Interval v.s. Average number of steps")
```

![plot of chunk assignment_4_plot](figure/assignment_4_plot-1.png) 


