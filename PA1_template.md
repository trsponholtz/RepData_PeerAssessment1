---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
fig.width: 6 
fig.height: 4
---


## Loading and preprocessing the data




```r
path <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/ReproducibleResearch/activity.csv"
activity_data <- read_csv(path,col_names=TRUE)
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```


## What is mean total number of steps taken per day?


```r
totsteps <- activity_data %>% group_by(date) %>% summarize(total = sum(steps,na.rm=TRUE))
     
hist(totsteps$total,
        breaks=20,
        main="Distribution of total number of steps per day",
        xlab="Total steps per day",
        border="black",
        col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean number of total steps per day is:

```r
mean(totsteps$total)
```

```
## [1] 9354.23
```

The median number of total steps per day is:

```r
median(totsteps$total)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
daily_avg <- activity_data %>% group_by(interval) %>% summarize(average = mean(steps,na.rm=TRUE))

plot(daily_avg$interval,daily_avg$average,
     type="l",
     xlab="Interval",
     ylab="Average Number of Steps",
     main="Average number of steps taken, by 5-minute interval",
     col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The maximum average number of steps taken in a 5-minute interval is:

```r
filter(daily_avg,average==max(daily_avg$average)) %>% select(interval)
```

```
## # A tibble: 1 x 1
##   interval
##      <dbl>
## 1      835
```

## Imputing missing values

### Replace missing values with mean

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

activity_data2 <- activity_data %>%
        group_by(interval) %>%
        mutate(
                steps = impute.mean(steps)
        )

totsteps2 <- activity_data2 %>% group_by(date) %>% summarize(total = sum(steps,na.rm=TRUE))

hist(totsteps2$total,
     breaks=20,
     main="Distribution of total number of steps per day, with single imputation",
     xlab="Total steps per day",
     border="black",
     col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Replacing the missing values with the mean value for the given interval has increased both the mean and median values compared to the analysis excluding the missing values.

## Are there differences in activity patterns between weekdays and weekends?


```r
par(mfrow=c(2,1)) 

activity_data$weekday <- (weekdays(activity_data$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))

daily_avg <- activity_data %>% group_by(weekday,interval) %>% summarize(average = mean(steps,na.rm=TRUE))

p1 <- plot(filter(daily_avg,weekday==0)$interval,filter(daily_avg,weekday==0)$average,
     type="l",
     xlab="Interval",
     ylab="Average Number of Steps",
     main="Average number of steps taken on weekends, by 5-minute interval",
     col="black")

p2 <- plot(filter(daily_avg,weekday==1)$interval,filter(daily_avg,weekday==1)$average,
           type="l",
           xlab="Interval",
           ylab="Average Number of Steps",
           main="Average number of steps taken on weekdays, by 5-minute interval",
           col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

On weekends (top graph), activity tends to increase later in the day and more gradually than on weekdays (bottom graph). Activity also appears to be more evenly distributed thoughout the day. The maximum average number of steps in a 5-minute interval is higher on weekdays.
