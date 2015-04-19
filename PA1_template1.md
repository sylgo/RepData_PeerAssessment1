# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library(data.table)
activity <- read.csv("activity.csv")
DT <- data.table(activity)
DT <- DT[complete.cases(DT),]
```

## What is mean total number of steps taken per day?

```r
steps_per_day <- DT[, sum(steps), by = date]
setnames(steps_per_day, c('date','steps'))
hist(steps_per_day[,steps])
```

![](PA1_template1_files/figure-html/unnamed-chunk-2-1.png) 

Mean steps per day:

```r
steps_per_day[, mean(steps)]
```

```
## [1] 10766.19
```

Median steps per day:

```r
steps_per_day[, median(steps)]
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
steps_per_interval <- DT[, mean(steps), by = interval]
setnames(steps_per_interval, c('interval','steps'))
plot(steps_per_interval, type="l")
```

![](PA1_template1_files/figure-html/unnamed-chunk-5-1.png) 

Interval with the the maximum number of steps

```r
steps_per_interval[, .SD[which.max(steps)]]
```

```
##    interval    steps
## 1:      835 206.1698
```

## Imputing missing values

Number of rows with missing data

```r
nrow(activity) - sum(complete.cases(activity))
```

```
## [1] 2304
```

Total number of steps per day after imputing the NA values (replaced with the mean for the same interval and day of the week)

```r
activity1 <- activity
activity1$day <- weekdays(as.Date(activity1[,2]))
for (i in 1:nrow(activity1)) {
  if (is.na(activity1[i, 1])) {
  activity1[i, 1] <- mean(activity1[which(activity1$day == activity1[i,4] & activity1$interval == activity1[i,3]),1], na.rm = TRUE)
  }
}
DT <- data.table(activity1)
steps_per_day <- DT[, sum(steps), by = date]
setnames(steps_per_day, c("date","steps"))
hist(steps_per_day[,steps])
```

![](PA1_template1_files/figure-html/unnamed-chunk-8-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

```r
activity1$weekday <- 'weekday'
activity1[which(activity1$day %in% c('Saturday','Sunday')),5] <- 'weekend'
DT <- data.table(activity1)
avg_steps <- DT[, mean(steps), by = list(interval, weekday)]
setnames(avg_steps, c('interval','weekday','steps'))
library(lattice)
library(datasets)
xyplot( steps ~ interval | weekday , data = avg_steps, type = "l", layout = c(1,2))
```

![](PA1_template1_files/figure-html/unnamed-chunk-9-1.png) 
