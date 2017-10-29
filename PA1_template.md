# Analysis of the activity data
Valeriu Prohnitchi  
October 29, 2017  



## R Markdown

This is my first R Markdown document. It answers the questions of the Peer-graded Assignment: Reproducible Research Course Project 1.

# Loading and preprocessing the data


```r
FileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
FileName<-"repdata%2Fdata%2Factivity.zip"
if (!file.exists(FileName)) {download.file(FileUrl, "data.zip")}
library(readr)
data<-read_csv("data.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```
# Mean total number of steps taken per day


```r
daily_steps<-tapply(data$steps,data$date,sum)
hist(daily_steps, main="Steps per day", col="green", xlab="number of steps", ylab = "frequency, %")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png,file="steps_per_day.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
mean_steps<-mean(daily_steps, na.rm = TRUE)
print(mean_steps)
```

```
## [1] 10766.19
```

```r
median_steps<-median(daily_steps, na.rm = TRUE)
print(median_steps)
```

```
## [1] 10765
```

The mean and median steps per day are roughly equal. The mean is calculated at 10766.19 steps/day and the median is 10765 steps/day.

#Daily activity pattern


```r
interv_steps<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
interv<-unique(data$interval)
plot(interv,interv_steps,type="l", col="red", main="average steps taken by 5 min interval", ylab = "average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png,file="average_steps_by_5interval.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
max_interv_step<-names(which(interv_steps==max(interv_steps)))
print(max_interv_step)
```

```
## [1] "835"
```

The 835th is the 5-min interval containing the maximum number of steps.

#Imputing missing values
The imputing strategy is replacing missing values with the average values calculated for the corresponding 5 minutes interval.

```r
missing_values<-sum(is.na(data$steps))
print(missing_values)
```

```
## [1] 2304
```

```r
#there are 2304 missing values
data$allsteps<-interv_steps
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
ndata<-mutate(data,steps=ifelse(is.na(steps),allsteps,steps))
ndata$allsteps<-NULL

ndaily_steps<-tapply(ndata$steps,ndata$date,sum)
hist(ndaily_steps,col="blue",breaks =100)
hist(daily_steps,col=rgb(1,0,0,0.5), breaks=100, add=TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png,file="imputed_steps_per_day.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
nmean_steps<-mean(ndaily_steps)
print(nmean_steps)
```

```
## [1] 10766.19
```

```r
nmedian_steps<-median(ndaily_steps)
print(nmedian_steps)
```

```
## [1] 10766.19
```

```r
impact<-(mean(ndaily_steps)-mean(daily_steps,na.rm = TRUE))/mean(daily_steps,na.rm = TRUE)*100
print(impact)
```

```
## [1] 0
```

Due to the imputing strategy used, apparently there are no differences between the central position indicators.

#Differences in activity patterns between weekdays and weekends

```r
ndata$weekday<-as.factor(weekdays(ndata$date))
levels(ndata$weekday)<-list(weekday=c("Friday","Monday","Thursday","Tuesday","Wednesday"),
                          weekend=c("Saturday","Sunday"))
dsteps <- aggregate(steps ~ interval + weekday, ndata, mean)
library(lattice)
xyplot(dsteps$steps ~ dsteps$interval|dsteps$weekday, main="Mean Daily Steps by 5-min Interval",xlab="5-min Interval", ylab="Mean Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png,file="Daily_Steps_Weekday_Weekend.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
