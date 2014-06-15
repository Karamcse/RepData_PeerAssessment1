# Reproducible Research: Peer Assessment 1


## 1) Loading and preprocessing the data

```r
if (!file.exists("../ProjectData/activity.csv")) {
    download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
        destfile = "../activity.zip", method = "auto")
    unzip("../activity.zip", exdir = "../ProjectData")
}
amd <- read.csv("../ProjectData/activity.csv", header = T, sep = ",")
str(amd)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Process/transform the date field in Activity monitoring data into a date format suitable for your analysis

```r
amd$date <- as.Date(amd$date)
str(amd)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## 2) What is mean total number of steps taken per day?
### i) Make a histogram of the total number of steps taken each day

```r
amdsteps <- tapply(amd$steps, amd$date, sum)
```


```r
library(reshape2)
amdmelt <- melt(amdsteps)
names(amdmelt) <- c("Date", "SumofSteps")
head(amdmelt)
```

```
##         Date SumofSteps
## 1 2012-10-01         NA
## 2 2012-10-02        126
## 3 2012-10-03      11352
## 4 2012-10-04      12116
## 5 2012-10-05      13294
## 6 2012-10-06      15420
```


```r

hist(amdmelt$SumofSteps, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Total Number of Steps per Day", ylab = "Frequency", col = "blue", 
    breaks = 30)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

### ii) Calculate and report the mean and median total number of steps taken per day

```r
mean(amdmelt$SumofSteps, na.rm = T)
```

```
## [1] 10766
```

```r
median(amdmelt$SumofSteps, na.rm = T)
```

```
## [1] 10765
```


## 3) What is the average daily activity pattern?

### i) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
amdavg <- tapply(amd$steps, amd$interval, mean, na.rm = T)
amdmelt_avg <- melt(amdavg)
names(amdmelt_avg) <- c("interval", "avg")
nrow(amdmelt_avg)
```

```
## [1] 288
```

```r
plot(avg ~ interval, data = amdmelt_avg, type = "l", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

### ii) which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
amdmelt_avg[amdmelt_avg$avg == max(amdmelt_avg$avg), ]
```

```
##     interval   avg
## 104      835 206.2
```


## 4) Imputing missing values

### i) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
colSums(is.na(amd))
```

```
##    steps     date interval 
##     2304        0        0
```

### ii) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
mean(amd$steps, na.rm = T)
```

```
## [1] 37.38
```


#### iii) Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
amdimpute <- amd
amdimpute$steps[is.na(amdimpute$steps)] <- mean(amdimpute$steps, na.rm = T)
colSums(is.na(amdimpute))
```

```
##    steps     date interval 
##        0        0        0
```


### iv) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
amdimputesteps <- tapply(amdimpute$steps, amdimpute$date, sum)
```


```r
library(reshape2)
amdmeltimpute <- melt(amdimputesteps)
names(amdmeltimpute) <- c("Date", "SumofSteps")
head(amdmeltimpute)
```

```
##         Date SumofSteps
## 1 2012-10-01      10766
## 2 2012-10-02        126
## 3 2012-10-03      11352
## 4 2012-10-04      12116
## 5 2012-10-05      13294
## 6 2012-10-06      15420
```


```r
hist(amdmeltimpute$SumofSteps, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Total Number of Steps per Day", ylab = "Frequency", col = "blue", 
    breaks = 30)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

### ii) Calculate and report the mean and median total number of steps taken per day

```r
mean(amdmeltimpute$SumofSteps, na.rm = T)
```

```
## [1] 10766
```

```r
median(amdmeltimpute$SumofSteps, na.rm = T)
```

```
## [1] 10766
```

#### (Mean = Median) after imputing missing value with mean value of steps. Now it became less skewed. 


## 5) Are there differences in activity patterns between weekdays and weekends?


```r
amdimpute$weekdays <- weekdays(amdimpute$date)

amdimpute$weeks[(amdimpute$weekdays == "Saturday" | amdimpute$weekdays == "Sunday")] <- "weekend"
amdimpute$weeks[!(amdimpute$weekdays == "Saturday" | amdimpute$weekdays == "Sunday")] <- "weekdays"
```



```r
library(plyr)
week_comp <- ddply(amdimpute, c("interval", "weeks"), function(x) apply(x[1], 
    2, mean))
head(week_comp)
```

```
##   interval    weeks steps
## 1        0 weekdays 7.007
## 2        0  weekend 4.673
## 3        5 weekdays 5.384
## 4        5  weekend 4.673
## 5       10 weekdays 5.140
## 6       10  weekend 4.673
```





```r
library(lattice)
xyplot(steps ~ interval | weeks, data = week_comp, type = "l", xlab = "Interval", 
    ylab = "Number of steps", layout = c(1, 2))
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

