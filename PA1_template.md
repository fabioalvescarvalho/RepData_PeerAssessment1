---
title: "PA1_template"
author: "fabiooalvescarvalho"
date: "24 de janeiro de 2016"
output: html_document
---


# 1.  Loading and preprocessing the data #

1.1 Load the data (i.e. read.csv())

```{r}

rm(list=ls())
setwd("C:/Git_Paste/RepData_PeerAssessment1/")
list.files()
unzip("activity.zip")
df <- read.csv("activity.csv")
head(df)
```

1.2 Process/transform the data (if necessary) into a format suitable for your analysis

```{r}

library(ggplot2)
library(plyr)

df$date <- as.Date(df$date)
data <- subset(df, !is.na(df$steps))
```

# 2. What is mean total number of steps taken per day? #


2.1 Sum steps by day ... and calculate mean and median


```{r}
total.steps.day <- tapply(data$steps, data$date, sum)
#steps_by_day <- aggregate(steps ~ date, data, sum) Other way
```

... create Histogram ...
```{r}
hist(total.steps.day, main = paste("Total Steps Each Day"), breaks = 20, col = "blue", xlab = "Number of Steps")
# other form to procedure
#steps.by.day <- aggregate(data$steps ~ data$date, FUN = sum)
#colnames(steps.by.day)<- c("Date", "Steps")
#hist(data$steps, breaks=5, xlab = "Steps", main = "Total Steps per Day")
#qplot(total.steps.day, binwidth=1000, xlab="Total Number of Steps by Day")
```

Calculate average number of steps taken each day was ...

```{r}
mean(total.steps.day, na.rm = T)
```

and the CV median number of steps taken each day was 10765 steps.

```{r}
median(total.steps.day, na.rm = T)

```



# 3. What is the average daily activity pattern?


3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

```



3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
averages[which.max(averages$steps),]
```


The maximum number of steps for a 5-minute interval was 206 steps and the 5-minute interval which had the maximum number of steps was the 835 interval


# 4. Imputing missing values



4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}

missing.na.data <- is.na(df$steps)
table(missing.na.data)

```


4.2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc


# Replace each missing value with the mean value of its 5-minute interval

```{r}
#fill.value <- function(steps, interval) {
#    filled <- NA
#    if (!is.na(steps))
#        filled <- c(steps)
#    else
#        filled <- (averages[averages$interval==interval, "steps"])
#    return(filled)
#}


data.impute <- df
ndx <- is.na(data.impute$steps)


```


4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
int.avg <- tapply(data$steps, data$interval, mean, na.rm=TRUE, simplify=T)
data.impute$steps[ndx] <- int.avg[as.character(data.impute$interval[ndx])]

```


4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

new.total.steps.day <- tapply(data.impute$steps, data.impute$date, sum, na.rm=TRUE, simplify=T)

hist(x=new.total.steps.day,
     col="red",
     breaks=20,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data imputed)")


mean(total.steps.day)

median(total.steps.day)

```


# 5. Are there differences in activity patterns between weekdays and weekends?



5.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
is.weekday <- function(d) {
    wd <- weekdays(d)
    ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(data.impute$date, is.weekday)
data.impute$wk <- as.factor(wx)
head(data.impute)


```



5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data



```{r}

wk.df <- aggregate(steps ~ wk + interval, data=data.impute, FUN=mean)



library(lattice)

xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk.df)


```


From the panel plot it looks like the weekday activities arise earlier than the weekends - weekday activities arise around 5~6am and weekend activities arise around 8am. We can also observe that from 10am to 5pm, the weekends have higher activity levels than the weekdays.