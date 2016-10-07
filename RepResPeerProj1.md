# RepResPeerProj1
KMcKeage  
October 5, 2016  
Loading and preprocessing the data


```r
mydata<-read.csv("activity.csv")
ndays<-unique(mydata$date)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
mydata<-filter(mydata,!is.na(steps))
date_sum<-group_by(mydata,date)
daily<-summarize(date_sum,sum(steps))
daysteps<-daily$'sum(steps)'
```

How are the steps per day distributed (range and form)?

```r
hist(daily$'sum(steps)', xlab="Number of Steps", main="Total Steps Per Day")
```

![](RepResPeerProj1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
This has a generally Gaussian (normal or bell curve) form, although a bit skewed.

What are the mean and median total number of steps taken per day?

```r
date_sum<-group_by(mydata,date)
daily<-summarize(date_sum,sum(steps))
daysteps<-daily$'sum(steps)'
mean(daysteps)
```

```
## [1] 10766.19
```

```r
median(daysteps)
```

```
## [1] 10765
```
So average (mean) daily steps = 10,766.19
Median steps per day is very similar, at 10,765
It makes sense that these two are very close since the histogram shows a 
generally Gaussian distribution form. 

What is the daily activity pattern? Look at a time series of steps over time intervals ...

```r
by_interval<-group_by(mydata,interval)
interval_mean<-summarize(by_interval,mean(steps))
plot(interval_mean$interval, interval_mean$'mean(steps)',type="l", xlab="Interval Number", ylab="Average Steps",main="Average Steps Per Sequential 5 Minute Interval")
```

![](RepResPeerProj1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Perhaps people are exercising in the morning, since the steps show a peak in the early intervals of the day, then are relatively steady throughout the rest of the day until evening. 

Five minute interval with highest maximum number of steps?

```r
sortint<-arrange(interval_mean,desc(interval_mean$'mean(steps)'))
highint<-sortint[1]$interval
highint2<-highint[1]
print(highint2)
```

```
## [1] 835
```
The highest interval is 835, which seems to be 8:35 a.m.

Imputing missing values - the strategy used is to substitute the mean for that variable for it's time interval. 

```r
mydata2<-read.csv("activity.csv")
sum(is.na(mydata2$steps))
```

```
## [1] 2304
```

```r
stepsnew<-ifelse (is.na(mydata2$steps),interval_mean$'mean(steps)',
                    mydata2$steps)
mydata3<-cbind(mydata2,stepsnew)
head(mydata3)
```

```
##   steps       date interval  stepsnew
## 1    NA 2012-10-01        0 1.7169811
## 2    NA 2012-10-01        5 0.3396226
## 3    NA 2012-10-01       10 0.1320755
## 4    NA 2012-10-01       15 0.1509434
## 5    NA 2012-10-01       20 0.0754717
## 6    NA 2012-10-01       25 2.0943396
```

```r
date_sum2<-group_by(mydata3,date)
daily2<-summarize(date_sum2,sum(stepsnew))
daysteps2<-daily2$'sum(stepsnew)'
mean(daysteps2)
```

```
## [1] 10766.19
```

```r
median(daysteps2)
```

```
## [1] 10766.19
```
There are 2304 missing values. After imputing values using the average for that time interval, looking at the printout we see that the NA for the first cases has been replaced with the interval mean in the variable "stepsnew". The mean and median for daily number of steps are now:
Mean:   10,766.19
Median: 10,766.19
The mean has not changed but the median changed slightly, to be equal to the mean. Given that the mean is an unbiased estimator, and it has not changed, imputing values for the missing data using this method has not changed our estimate. 

Histogram of total number of steps taken each day now that missing values have been imputed:

```r
date_sum2<-group_by(mydata3,date)
daily2<-summarize(date_sum2,sum(stepsnew))
daysteps2<-daily2$'sum(stepsnew)'
hist(daysteps2, xlab="Number of Steps", main="Total Steps Per Day")
```

![](RepResPeerProj1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
This looks very similar to the original histogram. 

Are there differences in activity patterns between weekdays and weekends?


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
weekdayno<-wday(mydata3$date)
mydataday<-cbind(mydata3,weekdayno)
weekdaynew<-ifelse (mydataday$weekdayno==1,mydataday$weekdayno+7,
                    mydataday$weekdayno)
weekdaynew<-weekdaynew-1
mydataday<-cbind(mydataday,weekdaynew)
weekend<-subset(mydataday,weekdaynew>5)
weekend<-data.frame(weekend)
workday<-subset(mydataday,weekdaynew<6)
workday<-data.frame(workday)
by_interval_wknd<-group_by(weekend,interval)
by_interval_wkday<-group_by(workday,interval)
interval_mean_wknd<-summarize(by_interval_wknd,mean(stepsnew))
interval_mean_wkday<-summarize(by_interval_wkday,mean(stepsnew))
par(mfrow=c(2,1),mar=c(4,4,1,1),oma=c(0,0,0,0))
plot(interval_mean_wkday$interval, interval_mean_wkday$'mean(stepsnew)',type="l", xlab="Interval", ylab="Avg # Steps",main="Weekday")
plot(interval_mean_wknd$interval, interval_mean_wknd$'mean(stepsnew)',type="l", xlab="Interval", ylab="Avg # Steps",main="Weekend")
```

![](RepResPeerProj1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
There are differences, with weekdays (Monday through Friday) showing concentrated patterns of steps in the morning, while weekends (Saturday and Sunday) shows steps occuring more uniformly throughout the day.  
