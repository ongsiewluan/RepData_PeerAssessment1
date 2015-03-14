
title: "Reproducible Research-PeerAssessment1"
output: html_document
=================================================

```{r PeerAssessment1, echo=TRUE}

##Loading and preprocessing the data
## The "activity.csv" file was extracted from the compressed "activity.zip"

##Load the data (i.e. read.csv())

  data <- read.csv("activity.csv", colClasses = c('integer','Date','factor'))

##What is mean total number of steps taken per day?
##For this part of the assignment, you can ignore the missing values in the dataset.
## We ignore the missing data by omitting it: filled_data

  filled_data <- na.omit(data)

## Make a histogram of the total number of steps taken each day

  library(ggplot2)
  totalsteps <- tapply(data$steps, data$date, FUN = sum, na.rm =TRUE)
  plot1 <- ggplot(filled_data, aes(date, steps)) + geom_bar(stat =   "identity",binwidth = .5) +
     labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date in year 2012", y = "Daily Total Number of Steps")
  print(plot1)

## Calculate and report the mean and median total number of steps taken per day
## Mean total number of steps:
  mean(totalsteps, na.rm=TRUE)

## Median total number of steps: 
  median(totalsteps, na.rm=TRUE)

## What is the average daily activity pattern?

  average_steps <- aggregate(filled_data$steps, list(interval = as.numeric(as.character(filled_data$interval))), FUN = "mean")
  names(average_steps)[2] <- "Avg.Steps"

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

  plot2 <- ggplot(average_steps, aes(interval, Avg.Steps)) + geom_line(color = "blue", size = 0.7) + labs(title = "Time Series Plot of the 5-minute Intervals", x = "5-minute intervals", y = "Average Number of Steps Taken")
  print(plot2)

##   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

 average_steps[average_steps$Avg.Steps == max(average_steps$Avg.Steps),]

## Imputing missing values

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

  missing_values <- is.na(data$steps)

## How many missing

  sum(missing_values)

##  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

  ImputData <- data
  for (i in 1:nrow(data)) {
    if (is.na(data$steps[i])) {
      ImputData$steps[i] <- average_steps[which(data$interval[i] == average_steps$interval), ]$Avg.Steps
    }
  }


## Create a new dataset that is equal to the original dataset but with the missing data filled in.
## The new dataset with missing data filled in : Imputdata

 totalsteps_imputed <- tapply(ImputData$steps, ImputData$date, FUN= sum)

## Make a histogram of the total number of steps taken each day and  

plot3 <- ggplot(ImputData, aes(date, steps)) + geom_bar(stat = "identity",binwidth = .5) +
        labs(title = "Histogram of Total Number of Steps Taken Each Day (Imputed Data)",x = "Date", y = "Total Number of Steps")
print(plot3)

## Calculate and report the mean and median total number of steps taken per day.

mean(totalsteps_imputed)

median(totalsteps_imputed)

## Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# With these imputed values for the NA figures, the mean and median of the total number of steps are higher than when the NAs are omitted.
# The impact of imputing missing data on the estimates of the total daily number of steps is that from the histograms, the days with empty bars are also filled out in the latter plot.

## Are there differences in activity patterns between weekdays and weekends?

## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


## Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

##   Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 




##Are there differences in activity patterns between weekdays and weekends?.



```
