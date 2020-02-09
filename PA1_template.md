---
title: "Reproducible Research: Peer Assignment 1"
author: "Qu?n Anh Dung"
date: "2/9/2020"

---

## Introduction

This is a R markdown document, created for the Courera Course. This assignment requires to write a R markdown document evidencing literate programming. 

There are 5 primary questions to be answered and the data provided to be worked upon, is called *activity morning data*.

### The Data

The data provided for use, is derived from a study whereupon a single individual wore a "personal activity monitoring device". The study says that:

" These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data. "

The device used in this particular dataset collects data on the number of steps taken by individual, in 5 minutes intervals. Two months of data, Oc/No 2012 are included. The variables measured include steps, date and interval (the interval in which the steps measurement was taken). Data in csv format with 17598 obs.

## Solving the Problem

### Ques 1: Loading and preprocessing the data

The data must be in the "Reproducible-Research-Assignment" wd and we should extract the data first. The object classes contained within each of the variables are defined, so as to speed up the reading process.

```{r, echo = TRUE}
unzip("repdata_data_activity.zip")
dat <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

We can see the dimensions and contents of this data:

```{r, echo= TRUE}
head(dat)
str(dat)
```

### Ques 2: Calculate the mean of total number of steps taken per day.
 
The question states any missing values in the data set can be ignored. From function summary that we used previously, it's already that there're NA values within the steps var, so we should remove now.

```{r, echo= T}
data <- na.omit(dat)
```

To calculate the total number of steps, we should separate the data for each day and then calculate sum of the each day. The aggregate function can complete both of these steps, and format the output in a tidy data frame.

```{r, echo= TRUE}
totalstep <- aggregate(steps ~ date, data, sum)
head(totalstep)
```

Creating exploratory plots are useful to be able to quickly see a view of all of the data. So here, we can see the histogram to indicate the frequency of total steps taken each day.

```{r, echo = TRUE}
paletteBlue <- colorRampPalette(c("skyblue", "darkblue", "skyblue"))
hist(totalstep$steps, xlab= "Number of Steps",
     main="Histogram of Total Number of Steps",
     col=paletteBlue(22), family="serif")
```

And then we calculate the mean and median values of the total number of steps taken per day.

```{r, message= FALSE}
library(dplyr)
Sum <- summarise(totalstep, mean = mean(totalstep$steps), median = median(totalstep$steps)) 
print(Sum)
```

### Ques 3: Calculate the average daily activity pattern.

To look at the average daily pattern, we can use another exploratory graph, this time a time series plot. We should use the aggregate function again and then averaged with the mean function.

```{r, echo= TRUE}
meanstep <- aggregate(steps ~ interval, data, mean)
head(meanstep)
```

The R plotting system is used to create a time series plot. Here, we can see: 

```{r, echo = TRUE}
plot(x= meanstep$interval, y= meanstep$steps, type = "l", 
     main= "Time Series Plot", 
     ylab= "Number of Steps", 
     xlab = "Interval (5 mins)", 
     col= "skyblue", lwd= 1.6)
```

The last part of this question asks "which five minute interval contains the maximum number of steps?". We can use max function to find out the max value from a numeric vertor.

```{r, echo = TRUE}
meanstep[grep(max(meanstep$steps), meanstep$steps), ]
```

### Ques 4: Imputing missing values

As the data can be confirmed to contain some NA values as some of the observations:

```{r, echo = TRUE}
anyNA(dat)
```

It is necessary to find out whether the NA values are more-so clustered to one area within the data. Looking at each of the variables:

```{r, echo = TRUE}
data.frame(steps = sum(is.na(dat$steps)),
           interval = sum(is.na(dat$interval)),
           date = sum(is.na(dat$date)))
```

It can be seen that all 2304 NA values are contained within the steps variable. Therefore an imputing strategy must be devised to replace all of these missing values with usable numeric measurements. To replace it, i decided to add mean value for the same interval and averaged across all days.

I will use **for** loop to achieve this.

```{r, echo = TRUE}
for(i in 1:17568) {
        if(is.na(dat[i, 1] == TRUE)) {
                dat[i, 1] <- meanstep[meanstep$interval %in% dat[i, 3], 2]
        }
}
head(dat)
```

Now that the NA values have been replaced, a histogram from the imputed data can be created.

```{r, echo= TRUE}
imputedtotalstep <- aggregate(steps ~ date, dat, sum)
head(imputedtotalstep)
```

Creating the histogram:

```{r, echo= TRUE}
paletteRed <- colorRampPalette(c("deeppink", "darkred", "deeppink"))
hist(imputedtotalstep$steps, 
     xlab="Number of Steps",
     main="Histogram of Total Number of Steps",
     col=paletteRed(27), family="serif")
```

We use **summarise** function again to find out the mean and median total number of steps.

```{r, echo= TRUE}
summa <- summarise(imputedtotalstep,
                   mean= mean(imputedtotalstep$steps),
                   median= median(imputedtotalstep$steps))
print(summa)
```
Compare two datasets:

```{r}
rbind(Sum, summa)
```

The values of the two data sets are very similar, if not exactly the same, due to the use of averaging functions when imputing the NA measurements.

So we use histogram to compare two datasets (imputed and non-imputed):

```{r, echo= TRUE}
par(mfrow= c(1,2))

hist(totalstep$steps, breaks = 20, xlab = "Number of Steps",
     col=paletteBlue(22), ylim = c(0,20), main= NULL, family= "serif")
hist(imputedtotalstep$steps, breaks = 20, xlab = "Number of steps",
     col=paletteRed(22), ylim= c(0,20), main = NULL, family="serif")
mtext("Histogram of Total Number of Steps, With/Without Imputed Values", 
      font = 2, family="serif", adj= 0.95)
```

It can be seen that the frequency of values increases in the second histogram, which is expected, due to the imputed values. 

More explanations for the differences between the non and imputed data sets can be seen by looking at the NA values grouped by their date variable.

```{r, results= 'hide'}
naDate <- dat[is.na(dat$steps), ]
table(naDate$date)
```

There're exactly 288 intervals measured for each day:

```{r}
length(unique(data$interval))
```

It is therefore shown by the above table, that in the initial data set, missing observations are due to entirely missed days, (8 of the days) where no measurements were made whatsoever. This therefore reinforces that the imputing technique used, of utilising average interval data, was likely more useful than potentially using average daily data.

### Ques 5: The difference in activity patterns between weekdays and weekends. 

We use imputed dataset to answer this prob.To help in answering this question, firstly a new factor variable should be created within the data frame. This should indicate whether each day is a "weekday" or a "weekend".

To achieve this, I used the weekdays function to automatically calculate the day of the week each day resided upon, (Monday, Tuesday, etc.) Next, I wrote a for loop, which would assign the factor value "weekend" to all rows it read as having the values "Saturday" or "Sunday", and assign "weekday" to the others.

```{r,echo= TRUE}
daysDat <- dat
daysDat$days <- weekdays(daysDat$date)
daysDat$weekday <- as.character(rep(0, times= 17568))
for(i in 1:17568){
        if(daysDat[i, 4] %in% c("Saturday", "Sunday")) {
                daysDat[i, 5] <- "weekend"
        } else {
                daysDat[i, 5] <- "weekday"
        }
}
daysDat$weekday <- factor(daysDat$weekday)
head(daysDat)
```

To compare the weekday and weekend data, and create two plots of the average number of steps taken per interval, the data has to be split into two groups of weekday/weekend data, using the newly created variable.

```{r}
weekdayDat <- daysDat[daysDat$weekday == "weekday", ]
weekendDat <- daysDat[daysDat$weekday == "weekend", ]
```

Then, we calculate the average:

```{r}
weekdaymean <- aggregate(steps ~ interval, weekdayDat, mean)
weekendmean <- aggregate(steps ~ interval, weekendDat, mean)
```

Finally, we use panel plot to compare two datasets:

```{r, echo = TRUE}
par(mfrow=c(2, 1), mar=c(4, 4.1, 3, 2.1))
plot(weekdaymean$interval, weekdaymean$steps, type="l",
     main="Time Series Plot of Average Steps Taken per Interval, for Weekdays",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps", family="serif",
     col="darkred", lwd=1.5, ylim=c(0, 230))
plot(weekendmean$interval, weekendmean$steps, type="l",
     main="Time Series Plot of Average Steps Taken per Interval, for Weekends",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps", family="serif",
     col="darkblue", lwd=1.5, ylim=c(0, 230))
```


