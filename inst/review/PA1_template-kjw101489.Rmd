# Reproducible Research: Peer Assessment 1
## Read in the data file and preprocess
ad <- read.csv("activity.csv")
steps <- ad$steps
date <- ad$date
interval <- ad$interval
## Building a Histogram
hist(tapply(steps, date, sum), xlab="Steps", ylab= "Days", breaks=10, main="Steps taken on a Daily basis")

## Calculate the Mean and Median Values
steps_day <- as.numeric(tapply(steps, date, sum))
mean_steps_day <- mean(steps_day, na.rm=TRUE)
median_steps_day <- median(steps_day, na.rm=TRUE)

## Compute the average daily activity pattern
interval <- as.factor(as.character(interval))
mean_int <- as.numeric(tapply(steps, interval, mean, na.rm=TRUE))
trueint <- data.frame(trueint = as.numeric(levels(interval)),mean_int)
trueint <- trueint[order(trueint$trueint),]

## Build the plot
plot(trueint$trueint, trueint$mean_int, type = "l", main = "Average steps per minute", xlab="Time of Day", ylab="Mean Steps")
aveintper <- trueint[order(trueint$mean_int, decreasing=TRUE),]
head(aveintper, n=1)

## Input Missing Values
missvals <- sum(is.na(steps))
steps[is.na(steps)] <- 37.38
summary(steps)
hist(tapply(newad$steps, newad$date, sum), xlab="Steps", ylab= "Days", breaks=10, main="Steps taken on a Daily basis")

newsteps_day <- as.numeric(tapply(newad$steps, newad$date, sum))
newmean_steps_day <- mean(newsteps_day, na.rm=TRUE)
newmedian_steps_day <- median(newsteps_day, na.rm=TRUE)
newad_new <- newad
newad_new$date <- as.Date(newad_new$date)
newad_new$day <- weekdays(newad_new$date) == "Sunday"  | weekdays(newad_new$date) == "Saturday"

newad_new_weekday <- newad_new[newad_new$day == FALSE, ]
newad_new_weekend <- newad_new[newad_new$day == TRUE, ]
#First create weekday and weekend variables
mean_weekday <- tapply(newad_new_weekday$steps, newad_new_weekday$interval, mean)
mean_weekend <- tapply(newad_new_weekend$steps, newad_new_weekend$interval, mean)
#then create two separate plots

plot(mean_weekday, type = "l", xlab="Time interval", ylab="Steps", main="Weekday activity", col="darkblue")
plot(mean_weekend, type="l", xlab="Time interval", ylab="Steps", main="Weekend activity", col="red")
