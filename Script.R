library(dplyr)

## 1. Code for reading in the dataset and/or processing the data
unzip("./activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")

## 2. Histogram of the total number of steps taken each day
pdata <- aggregate(data$steps, by=list(date=data$date), FUN=sum)
with(pdata, (plot(date,x, 
                  type = "h", 
                  main = "Total steps per day",
                  xlab = "Date", 
                  ylab = "Number of steps", 
                  lwd = 6, 
                  col = "blue")))

## 3. Mean and median number of steps taken each day
promedio <- mean(pdata$x, na.rm = TRUE)
media <- median(pdata$x, na.rm = TRUE)
print(paste("Mean =", round(promedio,1), "Median = ", media))

## 4. Time series plot of the average number of steps taken
qdata <- aggregate(data$steps, by=list(interval=data$interval), FUN=mean, na.rm = TRUE)
with(qdata, plot(interval, x, 
                 type = "l", 
                 main = "Average number of steps by interval",
                 xlab = "Interval", 
                 ylab = "Mean steps"))


## 5. The 5-minute interval that, on average, contains the maximum number of steps
maxinterval <- qdata[which.max(qdata$x),1]
print(paste("Five minute interval with max number of steps on average:", maxinterval))

## 6. Code to describe and show a strategy for imputing missing data
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

## 7. Histogram of the total number of steps taken each day after missing values are imputed
npdata <- aggregate(ndata$steps, by=list(date=data$date), FUN=sum)
with(npdata, (plot(date,x, 
                  type = "h", 
                  main = "Total steps per day",
                  xlab = "Date", 
                  ylab = "Number of steps", 
                  lwd = 6, 
                  col = "red")))
npromedio <- mean(npdata$x)
nmedia <- median(npdata$x)
print(paste("Mean =", round(npromedio,1), "Median = ", round(nmedia,1)))
print(paste("While the mean remained the same the median varied by", 
            round(nmedia-media, 3),
            "when imputting missing values"))

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends



## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
