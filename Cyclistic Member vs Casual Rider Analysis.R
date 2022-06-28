#Give credit where credit is due
#This script follows the "Divvy Exercise R Script" that can be found at
#https://docs.google.com/document/d/1TTj5KNKf4BWvEORGm10oNbpwTRk1hamsWJGj6qRWpuI/edit
#It is NOT strictly copy/paste, but without the starter script, I wouldn't have
#been able to do some of what's below.

# Load Libraries

library("tidyverse")
library("lubridate")
library("anytime")
library("ggplot2")

#set working directory
setwd("C:/Users/jj/Documents/Google Data Analytics Cert/capstone project/Cyclistic Member vs Casual Rider Analysis")

#read in data from all files

data_202205 <- read.csv('202205-divvy-tripdata.csv')
data_202204 <- read.csv('202204-divvy-tripdata.csv')
data_202203 <- read.csv('202203-divvy-tripdata.csv')
data_202202 <- read.csv('202202-divvy-tripdata.csv')
data_202201 <- read.csv('202201-divvy-tripdata.csv')
data_202112 <- read.csv('202112-divvy-tripdata.csv')
data_202111 <- read.csv('202111-divvy-tripdata.csv')
data_202110 <- read.csv('202110-divvy-tripdata.csv')
data_202109 <- read.csv('202109-divvy-tripdata.csv')
data_202108 <- read.csv('202108-divvy-tripdata.csv')
data_202107 <- read.csv('202107-divvy-tripdata.csv')
data_202106 <- read.csv('202106-divvy-tripdata.csv')

#next time, try this method for reading in multiple files
# https://www.youtube.com/watch?v=An1bUIg-nVM&t=2s

#review column names for each data file, to determine if they are all the same

colnames(data_202205)
colnames(data_202204)
colnames(data_202203)
colnames(data_202202)
colnames(data_202201)
colnames(data_202112)
colnames(data_202111)
colnames(data_202110)
colnames(data_202109)
colnames(data_202108)
colnames(data_202107)
colnames(data_202106)

#combine all monthly files into single data frame

all_trips <- bind_rows(data_202205, data_202204, data_202203, data_202202, 
                       data_202201, data_202112, data_202111, data_202110,
                       data_202109, data_202108, data_202107, data_202106)

#inspect newly created data frame

colnames(all_trips)
nrow(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)


# slice out part of the data frame to view (first attempt at view(all_trips) 
# crashed RStudio!!!)  No wonder, 5M records :)

all_trips_v0.1 <- slice(all_trips, 1:100)
view(all_trips_v0.1)

#inspect rideable_type and member_casual column values to ensure there are no 
#slight variations in spelling/abbreviation throughout the dataset

unique(all_trips$rideable_type)
unique(all_trips$member_casual)

#inspect start and end station names for slight variations
#create a list to view since there's over 1000 unique values

start_station_names <- sort(unique(all_trips$start_station_name))
view(start_station_names)

end_station_names <- sort(unique(all_trips$end_station_name))
view(end_station_names)

#check ride_id column for duplicated IDs

sum(duplicated(all_trips$ride_id))

#add columns to enable aggregating data

all_trips$date <- anydate(all_trips$started_at)
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$weekday <- format(as.Date(all_trips$date), "%A")
all_trips$month <- format(as.Date(all_trips$date), "%b")
all_trips$ridelength <- difftime(anytime(all_trips$ended_at), anytime(all_trips$started_at), units = "min")

#create and view a new data frame to view 1st 100 rows of all_trips

all_trips_v1 <- slice(all_trips,1:100)
view(all_trips_v1)

#convert ridelength column to numeric to enable calcs

all_trips$ridelength <- as.numeric(all_trips$ridelength)

#inspect min, max and mean for ridelength column

summary(all_trips$ridelength)

#create a new data frame to exclude ride_length less than 1, since view() showed  
#records with 0 mins and the summary() function showed min value is less than 0
#NOTE - 0 min ride lengths do NOT have the same start and stop times!!!

#ALSO - max value of 55944.15 (given a median of 11.13 and mean of 21.02) seems
#odd.  Arbitrarily sub-setting data for rides greater than 1 day and 1 week
#for closer look

all_trips_v2 <- all_trips[!(all_trips$ridelength < 1),]

#create new data frame to view trips longer than 1 week
all_trips_v3 <- all_trips[(all_trips$ridelength > 10080),]
View(all_trips_v3)

#create new data frame to view trips longer than 1 day
all_trips_v4 <- all_trips[(all_trips$ridelength > 1440),]
View(all_trips_v4)

#DESCRIPTIVE ANALYSIS

summary(all_trips_v2$ridelength)
sd(all_trips_v2$ridelength)
hist(all_trips_v2$ridelength)

#Max ride length of 55944.15 mins is troubling.  With only 4,338 rides greater than
#a day and 301 rides greater than a week, the mean is skewed slightly with these 
#data points in the set.  Greater than a day seems like atypical riders, given 
#there are 5M total records in the data set
#Pare down the data set to rides that aren't greater than a day (and not less than 0)

all_trips_v2.1 <- all_trips[!(all_trips$ridelength > 1440) & !(all_trips$ridelength < 1),]
                              

summary(all_trips_v2.1$ridelength)
sd(all_trips_v2.1$ridelength)
hist(all_trips_v2.1$ridelength)

#Still a significant spread in the data, 3rd quartile is only about 21 mins vs
#1440 mins  There is an incredibly long right tail in the distribution


#Calculate the mild outlier thresholds, to determine a suitable upper bound
#for the bulk of the eata and possibly show a different group of ride 
#characteristics

#creating a value so I can manually check the math the code is doing...
ridelength_IQR <- IQR(all_trips_v2.1$ridelength)

if(quantile(all_trips_v2.1$ridelength, probs = .25, na.rm = TRUE)-
  (1.5 * IQR(all_trips_v2.1$ridelength)) < 0)
  {
  lower_threshold <- 0
  }else {
    lower_threshold <- quantile(all_trips_v2.1$ridelength, probs = .25, na.rm = TRUE)-
      (1.5 * IQR(all_trips_v2.1$ridelength))
  }

upper_threshold <- quantile(all_trips_v2.1$ridelength, probs = .75, na.rm = TRUE) +
  (1.5 * IQR(all_trips_v2.1$ridelength))

#create data frames for both sides of the upper threshold, to enable
#a separate review

all_trips_v2.2 <- all_trips_v2.1[(all_trips_v2.1$ridelength < 42.5),]
all_trips_v2.3 <- all_trips_v2.1[!(all_trips_v2.1$ridelength < 42.5),]

#Descriptive Analysis

#compare mean, median, min and max for casual vs member riders, first for
#overall data set

aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual, FUN = mean)
aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual, FUN = median)
aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual, FUN = min)
aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual, FUN = max)

#Review rides by day of week

#I WILL have order!
all_trips_v2.1$weekday <- ordered(all_trips_v2.1$weekday, 
  levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual + all_trips_v2.1$weekday, FUN = mean)
  
#Compare mean, median, max and min for data subset - rides less than 42.5 mins

aggregate(all_trips_v2.2$ridelength ~ all_trips_v2.2$member_casual, FUN = mean)
aggregate(all_trips_v2.2$ridelength ~ all_trips_v2.2$member_casual, FUN = median)
aggregate(all_trips_v2.2$ridelength ~ all_trips_v2.2$member_casual, FUN = min)
aggregate(all_trips_v2.2$ridelength ~ all_trips_v2.2$member_casual, FUN = max)

#Review rides by day of week

all_trips_v2.2$weekday <- ordered(all_trips_v2.2$weekday, 
                                  levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2.2$ridelength ~ all_trips_v2.2$member_casual + all_trips_v2.2$weekday, FUN = mean)

#Compare mean, median, max and min for data subset - rides more than 42.5 mins

aggregate(all_trips_v2.3$ridelength ~ all_trips_v2.3$member_casual, FUN = mean)
aggregate(all_trips_v2.3$ridelength ~ all_trips_v2.3$member_casual, FUN = median)
aggregate(all_trips_v2.3$ridelength ~ all_trips_v2.3$member_casual, FUN = min)
aggregate(all_trips_v2.3$ridelength ~ all_trips_v2.3$member_casual, FUN = max)

#Review rides by day of week

all_trips_v2.3$weekday <- ordered(all_trips_v2.3$weekday, 
                                  levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2.3$ridelength ~ all_trips_v2.3$member_casual + all_trips_v2.3$weekday, FUN = mean)

#CREATE PLOTS TO VISUALIZE DATA

#number of rides by day and by member type

all_trips_v2.1 %>% 
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides_v2.1 = n()) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Day of week
  and Rider Type", subtitle = "All Rides")

#average ride length by day and by member type

all_trips_v2.1 %>%
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average_duration_v2.1 = mean(ridelength)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Average Ride Duration by Day of week
  and Rider Type" ,subtitle = "All Rides")

#number of rides by day and by member type (data set v2.2, rides less than 
#42.5 minutes, the upper threshold for outliers in the overall data set)

all_trips_v2.2 %>% 
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides_v2.2 = n()) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides_v2.2, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Day of week
  and Rider Type", subtitle = "v2.2 - Rides less than 42.5 mins")

#average ride length by day and by member type

all_trips_v2.2 %>%
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average_duration_v2.2 = mean(ridelength)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration_v2.2, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Average Ride Duration by Day of week
  and Rider Type" ,subtitle = "v2.2 - Rides less than 42.5 mins")

#number of rides by day and by member type (data set v2.3, rides more than 
#42.5 minutes, the upper threshold for outliers in the overall data set)

all_trips_v2.3 %>% 
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides_v2.3 = n()) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides_v2.3, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Day of week
  and Rider Type",subtitle = "v2.3 - Rides more than 42.5 mins")

#average ride length by day and by member type

all_trips_v2.3 %>%
  mutate(day_of_week = wday(anydate(started_at), label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(average_duration_v2.3 = mean(ridelength)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration_v2.3, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Average Ride Duration by Day of week
  and Rider Type", subtitle = "v2.3 - Rides more than 42.5 mins")

#Ride Count by Rideable Type and Member Type - All Rides

all_trips_v2.1 %>% 
  mutate(rideable_type, label = TRUE) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(number_of_rides_v2.1 = n()) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Rideable Type
  and Rider Type", subtitle = "All Rides")

#Ride Length by Rideable Type and Member Type - All Rides
all_trips_v2.1 %>%
  mutate(rideable_type, label = TRUE) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(average_duration_v2.1 = mean(ridelength)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = average_duration_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Average Ride Duration by Rideable Type and Rider Type", 
                                      subtitle = "All Rides")

#Overall View of Members vs Casual Ride Count

all_trips_v2.1 %>% 
  group_by(member_casual) %>% 
  summarize(number_of_rides_v2.1 = n()) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, y = number_of_rides_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Rider Type", subtitle = "All Rides")


#Give the data frame order by month for the following chart
all_trips_v2.1$month <- ordered(all_trips_v2.1$month, 
   levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec"))

all_trips_v2.1 %>% 
  mutate(month, label = TRUE) %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides_v2.1 = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides_v2.1, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Cyclistic Ride Count by Month
  and Rider Type", subtitle = "All Rides")

#create .csv files for visualizations in programs other than R

ridecounts_day_type <- aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual + all_trips_v2.1$weekday, FUN = length)
write.csv(ridecounts_day_type, file = '~ride_count_day_type.csv')

ridelengths_day_type <- aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual + all_trips_v2.1$weekday, FUN = mean)
write.csv(ridelengths_day_type, file = '~avg_ride_length_day_type.csv')

ridelengths_month_type <- aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual + all_trips_v2.1$month, FUN = mean)
write.csv(ridelengths_month_type, file = '~ride_length_month_type.csv')

ridelengths_rideable_type <- aggregate(all_trips_v2.1$ridelength ~ all_trips_v2.1$member_casual + all_trips_v2.1$rideable_type, FUN = mean)
write.csv(ridelengths_rideable_type, file = '~ride_length_rideable_type.csv')
