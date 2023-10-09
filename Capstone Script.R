#install packages to work with data frame
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(skimr)
library(tidyr)
library(dplyr)
library(lubridate)

#upload packages for visualization and create visualization
install.packages("ggplot2")
library(ggplot2)

#set directory
setwd("/Users/yuspe/Documents/Interview/Project/Capstone project/Data/Rides")

# merge all files from this directory
df <- list.files(path='C:/Users/yuspe/Documents/Interview/Project/Capstone project/Data/Rides') %>% 
lapply(read_csv) %>% 
bind_rows

#show all my data
View(df)

#to see information about missing values, type of values and columns
skim_without_charts(df)

#to see all names of columns and types
glimpse(df)

#calculate new columns trip_time, day_week, month_trip, year_trip and add them to data frame
calc_df <- df %>%
  mutate(trip_time = as.numeric(difftime(ended_at,started_at, unit="mins"))) %>%
  mutate(day_week = wday(started_at)) %>%
  mutate(month_trip = month(started_at)) %>%
  mutate(year_trip = year(started_at))

glimpse(calc_df)

#delete rows with trip_time less or equal 1 minute
ind <- with(calc_df, (trip_time <= 1))
ind
clear_df <- calc_df[!ind, ]

glimpse(clear_df)

#to see new data frame
View(clear_df)
skim_without_charts(clear_df)

#create new data frame and drop columns which has n/a and we can't use them
part_df <- clear_df %>%
  select(ride_id, rideable_type, member_casual, trip_time, day_week, month_trip, year_trip)

View(part_df)
glimpse(part_df)

#create summary csv file to upload in Tableau for visualization
write_csv(part_df, "C:/Users/yuspe/Documents/Interview/Project/Capstone project/Cyclistic.csv")

#group data
part_df %>% group_by(year_trip, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = mean(trip_time), min_trip = min(trip_time), max_trip = max(trip_time), num_trip = n())
part_df %>% group_by(month_trip, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(as.numeric(trip_time)), mean_trip = mean(as.numeric(trip_time)), min_trip = min(as.numeric(trip_time)), max_trip = max(as.numeric(trip_time)), num_trip = n())
part_df %>% group_by(day_week, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = mean(trip_time), min_trip = min(trip_time), max_trip = max(trip_time), num_trip = n())
part_df %>% group_by(rideable_type, member_casual) %>% drop_na() %>% summarise(sum_trip = sum(trip_time), mean_trip = mean(trip_time), min_trip = min(trip_time), max_trip = max(trip_time), num_trip = n())

#mean trip by day of week and by type of customer
part_df %>% filter(!is.na(trip_time)) %>% group_by(day_week, member_casual) %>% summarise(mean_trip = mean(trip_time)) %>%
  ggplot(aes(x=day_week, y=mean_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#max trip by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(max_trip = max(trip_time)) %>%
  ggplot(aes(x=day_week, y=max_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#max trip by month and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(max_trip = max(trip_time)) %>%
  ggplot(aes(x=month_trip, y=max_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#min trip by day of week and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(min_trip = min(trip_time)) %>%
  ggplot(aes(x=month_trip, y=min_trip)) + geom_bar(stat = "identity") + facet_wrap(~member_casual)

#1.mean duration of trips by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(mean_trip = mean(trip_time)) %>%
  ggplot(aes(x=day_week, y=mean_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
    label = c(0, 5, 10, 15, 20, 25, 30, 35)
    ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  ) +
  ggtitle("Average duration of trips by day of week") +
  geom_text(
    aes(x=day_week, y=mean_trip, label=ceiling(mean_trip)), position=position_dodge(width=0.9), vjust=5
    )

#2.mean duration of trips by month and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(mean_trip = mean(trip_time)) %>%
  ggplot(aes(x=month_trip, y=mean_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30),
    label = c(0, 5, 10, 15, 20, 25, 30)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  ggtitle("Average duration of trips by month") +
  geom_text(
    aes(x=month_trip, y=mean_trip, label=ceiling(mean_trip)), position=position_dodge(width=0.9), vjust=5
  )

#3.mode of days of week by type of customer
ggplot(part_df, aes(day_week, fill = member_casual)) + 
  xlab("Days of Week") + ylab("Count") + geom_bar(position = "dodge")+
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  ) +
  scale_y_continuous(
    breaks = c(0, 125000, 250000, 375000, 500000, 625000),
    label = c(0, 125000, 250000, 375000, 500000, 625000)
  ) +
  ggtitle("Mode of days of week")
  
#4.number of trips by day of week and by type of customer
part_df %>% group_by(day_week, member_casual) %>% summarise(num_trip = n()) %>%
  ggplot(aes(x=day_week, y=num_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 125000, 250000, 375000, 500000),
    label = c(0, 125000, 250000, 375000, 500000)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    label = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  ) +
  ggtitle("Number of trips by day of week") +
  geom_text(
    aes(x=day_week, y=num_trip, label=ceiling(num_trip)), position=position_dodge(width=0.9), vjust=10, size=3
  )

#5.number of trips by month and by type of customer
part_df %>% group_by(month_trip, member_casual) %>% summarise(num_trip = n()) %>%
  ggplot(aes(x=month_trip, y=num_trip, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = c(0, 250000, 500000, 750000, 1000000),
    label = c(0, 250000, 500000, 750000, 1000000)
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  ggtitle("Number of trips by month")+
  geom_text(
    aes(x=month_trip, y=num_trip, label=ceiling(num_trip)), position=position_dodge(width=0.9), vjust=-0.25, size=3
  )
