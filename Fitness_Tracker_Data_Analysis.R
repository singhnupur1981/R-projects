library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)

daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")

head(daily_activity)
head(sleep_day)

is_null(daily_activity$Id)
is_null(daily_activity$ActivityDate)
is_null(daily_activity$TotalSteps)
is_null(daily_activity$TotalDistance)
is_null(daily_activity$TrackerDistance)
is_null(daily_activity$SedentaryActiveDistance)
is_null(daily_activity$Calories)


is_null(sleep_day$Id)
is_null(sleep_day$SleepDay)
is_null(sleep_day$TotalSleepRecords)
is_null(sleep_day$TotalMinutesAsleep)
is_null(sleep_day$TotalTimeInBed)


sum(duplicated(daily_activity))
sum(duplicated(sleep_day))

sleep_day <- sleep_day %>%
  distinct()

sum(duplicated(sleep_day))


daily_activity <- clean_names(daily_activity)
sleep_day <- clean_names(sleep_day)

head(daily_activity)
head(sleep_day)

sleep_day <- sleep_day %>%
  mutate (sleep_day = as.Date(sleep_day, format = "%m/%d/%Y"))

head(sleep_day)




daily_activity <- daily_activity %>%
  mutate (activity_date = as.Date(activity_date, format = "%m/%d/%Y"))

head(daily_activity)


n_distinct(daily_activity$id)
n_distinct(sleep_day$id)

nrow(daily_activity)
nrow(sleep_day)


daily_activity %>%
  select(total_steps,total_distance,very_active_minutes,fairly_active_minutes, lightly_active_minutes, sedentary_minutes, calories)%>%
  summary()

sleep_day%>%
  select(total_sleep_records,total_minutes_asleep,total_time_in_bed)%>%
  summary()


ggplot(data=daily_activity, aes(x=total_steps,y=sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
#`geom_smooth()` using formula 'y ~ x'

library("ggpubr")
test <- cor.test(daily_activity$sedentary_minutes, daily_activity$total_steps)
test

ggplot(data=sleep_day, aes(x= total_minutes_asleep,y=total_time_in_bed))+ 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

#`geom_smooth()` using formula 'y ~ x'


test <- cor.test(sleep_day$total_minutes_asleep,sleep_day$total_time_in_bed)
test

head(daily_activity)

head(sleep_day)


data1 <- daily_activity                                        
data1$weekday <- weekdays(as.Date(data1$activity_date))                 
head(data1)


data2 <- data1 %>%
  group_by(weekday) %>%
  summarize (total_steps = mean(total_steps)) 


data2$weekday <- ordered(
  data2$weekday,
  levels=c("Monday", "Tuesday", "Thursday", "Wednesday", "Friday", "Saturday", "Sunday"))


head(data2)

data1$weekday <- factor(data1$weekday , levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

boxplot(total_steps~weekday,
        data=data1,
        main="Number of steps per a day of the week",
        xlab="Weekdays",
        ylab="Number of steps",
        col="lightblue",
        border="black"
)