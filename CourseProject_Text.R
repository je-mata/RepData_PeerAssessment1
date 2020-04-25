#Reproducible Reseearch, Course Project No.1

##Loading package libraries and downloading data

#load package libraries
library(dplyr)
library(ggplot2)

#create data folder, download data & unzip file 
if(!file.exists("./DataSets")){dir.create("./DataSets")}
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("./DataSets/repdata-data-activity.zip")){
        download.file(URL,destfile="./DataSets/repdata-data-activity.zip", method="curl")
        #Unzip Files
        unzip(zipfile="./DataSets/repdata-data-activity.zip",exdir="./DataSets")
}

#View list of the file(s)
DataFiles <-list.files("./DataSets", recursive=TRUE)
DataFiles

##Loading data into R and preprocessing

#Create data frame object
data <- tbl_df(read.csv("./DataSets/activity.csv"))
data

#Clean data
#Turn date column into date variables and interval column into time values
#and create a new colun with date and time in POSIXct format
data$date <- as.Date(as.character(data$date))
data$interval <- substr(as.POSIXct(sprintf("%04.0f", data$interval), format='%H%M'), 12, 16)
data$date.time <- as.POSIXlt(paste(data$date, data$interval))

##What is mean total number of steps taken per day?
## 1. Calculate the total number of steps taken per day

#Aggregate sum of steps taken by date
StepsbyDay <- aggregate(steps ~ date, data, sum)

#We can viusalize this by plotting on bar chart
plot1 <- ggplot(StepsbyDay, aes(date, steps))
plot1 <- plot1 + geom_col() +
        xlab("Date") +
        ylab("Steps") +
        ggtitle("Steps taken per day") +
        theme(axis.text.x = element_text(angle = 90))+
        scale_x_date(breaks = as.Date(c(StepsbyDay$date)), date_labels = "%b-%d")
plot1

#Or simply view in a table 
StepsbyDay$date <- format(StepsbyDay$date, "%b-%d")
StepsbyDay

## 2.Make a histogram of steps taken per day
plot2 <- qplot(steps, data = StepsbyDay, bins = 5)
plot2 <- plot2 +
        xlab("Steps") +
        ylab("No. of Days") +
        ggtitle("Frequency of steps taken per day")
plot2

## 3.Calculate and report the mean and median of the total number of steps taken per day
summary(StepsbyDay$steps)
# We see that the mean mean is 10,766 steps per day and the median is 10,765
# steps per day

##What is the average daily activity pattern?
##1. Make a time series plot of the 5-minute interval (x-axis) and the average 
##number of steps taken, averaged across all days (y-axis)

#Aggregate by time interval and add a column to identify numericaly each
#timestamp
StepsbyTimeInterval <- aggregate(steps ~ interval, data, mean)
StepsbyTimeInterval$id <- c(1:288)

#Plot time series
plot3 <- ggplot(StepsbyTimeInterval, aes(id, steps))
plot3 <- plot3 + geom_line() +
        scale_x_continuous(labels = c("0:00", "8:15", "16:35", "")) +
        xlab("Time of Day") +
        ylab("steps") +
        ggtitle("Average steps according to time of day")
plot3

##2Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?

#Get max by subsetting the table we used for plotting with table with the 
#aggregated means of intervals
maxinterval <- subset(StepsbyTimeInterval, StepsbyTimeInterval$steps == max(StepsbyTimeInterval$steps))
maxinterval

#206 is the maximum average steps for one 5 minute interval, and it's the 8:35am
#interval

#Add this to our plot to verify visually
plot3 <- plot3 + 
        geom_point(x = maxinterval$id, y = maxinterval$steps, color = "orange") +
        geom_text(x = maxinterval$id + 40, y = maxinterval$steps, 
                  label = "206 steps - 8:35am")
plot3 


##Impute Na values

##1.Calculate and report the total number of missing values in the## dataset 
##(i.e. the total number of rows with NAs)

#Explore amount of NA values per column
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))
#NA values only in the steps column, therefore there are 2,304 missing values.

#2. Strategy for imputing missing values. We will create a new data set and add
#column with mean number of steps for each interval from interval means table 
data2 <- data
data2 <- merge(data2, StepsbyTimeInterval, by = "interval")
data2$date.time <- as.POSIXct(data2$date.time)
data2 <- tbl_df(data2)
data2 <- arrange(data2, date.time)
names(data2) <- c("interval", "steps", "date", "date.time", "interval.mean", "interval.id")


##3. Create a new dataset that is equal to the original dataset but with the 
##missing data filled in.

data2$steps <- ifelse(is.na(data2$steps), data2$interval.mean, data2$steps)
data2 <- data2[,c("steps","date", "interval", "date.time")]

##4. Make a histogram of the total number of steps taken each day and Calculate 
##and report the mean and median total number of steps taken per day. Do these 
##values differ from the estimates from the first part of the assignment? What 
##is the impact of imputing missing data on the estimates of the total daily 
##number of steps?


#Aggregate sum of steps taken by date
StepsbyDay2 <- aggregate(steps ~ date, data2, sum)

#We can viusalize this by plotting on bar chart.
plot4 <- qplot(steps, data = StepsbyDay2, bins = 5)
plot4 <- plot4 +
        xlab("Steps") +
        ylab("No. of Days") +
        ggtitle("Frequency of steps taken per day - Imputing NA's")
plot4

#and compare it with our initial histogram

plot2

#Compare means and median
summary(StepsbyDay$steps)
summary(StepsbyDay2$steps)

#The impact of imputing the data was barely noticeable on mean and median, however
#we did notice a change in frequencies, with about 8 more days registering in the
#10k steps per day range. 

##Are there differences in activity patterns between weekdays and weekends?

##1. Create a new factor variable in the dataset with two levels – “weekday” and 
##“weekend” indicating whether a given date is a weekday or weekend day.

data2$weekday <- weekdays(data2$date.time)
data2$weekday <- ifelse(data2$weekday == "Saturday" | data2$weekday == 
                                "Sunday", "weekend", "weekday")

##2. Make a panel plot containing a time series plot (i.e. type = "l" of the 
##5-minute interval (x-axis) and the average number of steps taken, averaged 
##across all weekday days or weekend days (y-axis)

#Aggregate by time interval and add a column to identify numerically each
#timestamp
weekdays <- 
        aggregate(steps ~ interval, subset(data2, data2$weekday == "weekday"), mean)
weekdays$id <- c(1:288)
weekdays$type <- "weekday" 
weekends <-
        aggregate(steps ~ interval, subset(data2, data2$weekday == "weekend"), mean)
weekends$id <- c(1:288)
weekends$type <- "weekend"
StepsbyTimeInterval2 <- rbind(weekdays, weekends)

#Plot time series
plot5 <- ggplot(StepsbyTimeInterval2, aes(id, steps, color = type))
plot5 <- plot5 + geom_line() +
        scale_x_continuous(labels = c("0:00", "8:15", "16:35", "")) +
        xlab("Time of Day") +
        ylab("steps") +
        ggtitle("Average steps according to time of day") +
        facet_grid(type~.) + 
        theme(legend.position = "none")
plot5


