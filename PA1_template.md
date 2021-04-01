---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

```r
library(dplyr)
library(tidyselect)
library(lattice)
library(knitr)
unzip("./activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
pdata <- aggregate(data$steps, by=list(date=data$date), FUN=sum)
with(pdata, (plot(date,x, 
                  type = "h", 
                  main = "Total steps per day",
                  xlab = "Date", 
                  ylab = "Number of steps", 
                  lwd = 6, 
                  col = "blue")))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
promedio <- mean(pdata$x, na.rm = TRUE)
media <- median(pdata$x, na.rm = TRUE)
print(paste("Mean =", round(promedio,1), "Median = ", media))
```

```
## [1] "Mean = 10766.2 Median =  10765"
```

## What is the average daily activity pattern?

```r
qdata <- aggregate(data$steps, by=list(interval=data$interval), FUN=mean, na.rm = TRUE)
with(qdata, plot(interval, x, 
                 type = "l", 
                 main = "Average number of steps by interval",
                 xlab = "Interval", 
                 ylab = "Mean steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maxinterval <- qdata[which.max(qdata$x),1]
print(paste("Five minute interval with max number of steps on average:", maxinterval))
```

```
## [1] "Five minute interval with max number of steps on average: 835"
```


## Imputing missing values

```r
missV <- sum(is.na(data$steps))
print(paste("Total number of missing values:", missV))
fillNA <- numeric()
for (i in 1:nrow(data)){
    obs <- data[i, ]
    if (is.na(obs$steps)){
        steps <- subset(qdata, interval == obs$interval)$x
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
ndata <- data
ndata$steps <- fillNA
npdata <- aggregate(ndata$steps, by=list(date=data$date), FUN=sum)
with(npdata, (plot(date,x, 
                  type = "h", 
                  main = "Total steps per day",
                  xlab = "Date", 
                  ylab = "Number of steps", 
                  lwd = 6, 
                  col = "red")))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
npromedio <- mean(npdata$x)
nmedia <- median(npdata$x)
print(paste("Mean =", round(npromedio,1), "Median = ", round(nmedia,1)))
```

```
## [1] "Mean = 10766.2 Median =  10766.2"
```

```r
print(paste("While the mean remained the same the median varied by", 
            round(nmedia-media, 3),
            "when imputting missing values"))
```

```
## [1] "While the mean remained the same the median varied by 1.189 when imputting missing values"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
daynames <- weekdays(ndata$date)
daynames <- gsub("lunes", "Weekday", daynames)
daynames <- gsub("martes", "Weekday", daynames)
daynames <- gsub("miércoles", "Weekday", daynames)
daynames <- gsub("jueves", "Weekday", daynames)
daynames <- gsub("viernes", "Weekday", daynames)
daynames <- gsub("sábado", "Weekend", daynames)
daynames <- gsub("domingo", "Weekend", daynames)
dias <- cbind(daynames, ndata)
resumen <- aggregate(steps ~ interval + daynames, dias, FUN = mean)
plot <- xyplot(steps ~ interval | daynames, data = resumen, 
       layout = c(1, 2), 
       type = "l", 
       xlab = "Interval", 
       ylab = "Number of steps", 
       main = "Average steps by interval")
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
