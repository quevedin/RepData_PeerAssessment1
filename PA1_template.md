# Reproducible Research: Peer Assessment 1
## setwd("C:/Users/Lucas/Downloads/RepData_PeerAssessment1")

## Loading and preprocessing the data

```r
library(readr)
activity <- read_csv("activity.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

```r
#View(activity)


## What is mean total number of steps taken per day?
suma<- aggregate(steps ~ date, activity, sum) 

## Plot using ggplot2
library(ggplot2)
ggplot(suma, aes(steps)) + 
  geom_histogram(fill = "steelblue2", colour = "steelblue4", 
    breaks = c(0, 5000, 10000, 15000, 20000, 25000)) + 
    labs(y = expression("frequency")) + 
    labs(x = expression("number of steps per day")) + 
    labs(title = expression("Fig 1")
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
med1= mean(suma$steps)

med2= median(suma$steps)

## What is the average daily activity pattern?
by_interval <- aggregate(steps ~ interval, data = activity, FUN = function(x) {
    mean(x, na.rm = TRUE)
})

## Time series plot
ggplot(by_interval, aes(interval, steps)) + geom_line(colour = "steelblue4", 
    lwd = 2) + labs(title = expression("Fig 2"))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
## Imputing missing values
na<-sum(is.na(activity))
rate<-paste(round(100*(na/nrow(activity)), 3), "%")

for (i in 1:length(activity$steps)) {
    if (is.na(activity[i, 1])) {
        
## Corresponding 5-minute interval, computed before
steps_average <- subset(by_interval, by_interval$interval == as.numeric(activity[i, 
            3]))$steps
        
        ## Replaces the value
        activity[i, 1] <- steps_average
    } else {
        activity[i, 1] <- activity[i, 1]
    }
    activity
}

by_date <- aggregate(steps ~ date, data = activity, sum)

library(ggplot2)
ggplot(by_date, aes(steps)) + geom_histogram(fill = "steelblue2", colour = "steelblue4", 
    breaks = c(0, 5000, 10000, 15000, 20000, 25000)) + labs(y = expression("frequency")) + 
    labs(x = expression("number of steps per day")) + labs(title = expression("Fig 3"))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
med1i=mean(by_date$steps)
## Are there differences in activity patterns between weekdays and weekends?
```
