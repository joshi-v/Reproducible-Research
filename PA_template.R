library(dplyr)
library(ggplot2)
theme(plot.title = element_text(hjust = 0.5))
# library(xtable)

# 1, download file, read and then delete it
setwd("C:\\Users\\vivek\\Documents\\Data Science\\Coursera\\05 - Reproducible Research\\Lecture 02")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "project.zip")
df <- read.csv(unz("project.zip", "activity.csv"))
file.remove("project.zip")
names(df)


#2 histogram of total steps
dplyr.tot <- df %>%
  group_by(date) %>%
  summarise(daily = sum(steps, na.rm = TRUE))

# can use tapply too
# total.steps <- tapply(df$steps, df$date, FUN=sum, na.rm=TRUE, simplify = TRUE)
ggplot(dplyr.tot, aes(daily)) + 
  geom_histogram(binwidth = 500, fill="orange", colour="black") +
  xlab("Number of Steps Per Day") + 
  ylab("Frequency") +
  ggtitle("Frequency count of Steps per day with NA removed") + 
  theme(plot.title = element_text(hjust = 0.5))

# qplot(total.steps, binwidth=500, xlab="total number of steps taken each day")

# 3, mean and median of total number of steps per day is

cat("Mean of total steps per day is:", mean(dplyr.tot$daily))
cat("Median of total steps per day is:", median(dplyr.tot$daily))

# missing number of steps is

cat("Number of NAs in the steps columns is:", sum(is.na(df$steps)))

#4, time series plots of average steps taken
df.avg.steps <- df %>%
  group_by(interval) %>%
  summarise(avg.steps = mean(steps, na.rm = TRUE))

ggplot(df.avg.steps, aes(interval, avg.steps)) + geom_line() +
  xlab("Interval") + ylab("Avg. Daily Steps") +
  ggtitle("Average Daily Steps vs. Daily Time Interval") + 
  theme(plot.title = element_text(hjust = 0.5))

# 5, max number of steps is
# this will give the index of df where the max value is
which.max(df.avg.steps$avg.steps)

df.avg.steps$interval[which.max(df.avg.steps$avg.steps)]

# to impute missing data will create and use a copied dataframe
df.nona <- df # copy the dataframe, will clean and use...
where.nas <- is.na(df.nona$steps)
# compute median over interval, since data has "wide" distribution.
impute.time <- tapply(df.nona$steps, df.nona$interval, median, na.rm = TRUE, simplify = TRUE)
df.nona$steps[where.nas] <- impute.time[as.character(df.nona$interval[where.nas])]
# check for no NAs
sum(is.na(df.nona))

#7, plot the histogram after missing values are imputed above
dplyr.nona.tot <- df.nona %>%
  group_by(date) %>%
  summarise(daily = sum(steps, na.rm = TRUE))

# can use tapply too
# total.steps <- tapply(df$steps, df$date, FUN=sum, na.rm=TRUE, simplify = TRUE)
ggplot(dplyr.nona.tot, aes(daily)) + 
  geom_histogram(binwidth = 500, fill="orange", colour="black") + 
  xlab("Number of Steps Per Day") + 
  ylab("Frequency") +
  ggtitle("Frequency count of Steps per day with NA imputed") + 
  theme(plot.title = element_text(hjust = 0.5))

# 8, onto panel plot
# convert date to Date type so can use weekday
df.nona$date <- as.Date(df.nona$date)

df.nona <- df.nona %>%
  mutate(daytype = ifelse(weekdays(df.nona$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
  
df.nona <- df.nona %>%
  group_by(interval) %>%
  mutate(avg.steps = mean(steps, na.rm = TRUE))

ggplot(df.nona, aes(x =interval , y=steps, color=daytype)) +
  geom_line() +
  labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
  facet_wrap(~ daytype, ncol = 1, nrow=2) + 
  theme(plot.title = element_text(hjust = 0.5))
