#Music therapy, or MT, is the use of music to improve an 
#individual's stress, mood, and overall mental health. 
#MT is also recognized as an evidence-based practice, using music as a 
#catalyst for "happy" hormones such as oxytocin.

#This is an analysis of the music preference of individuals of different ages,
#and the effect this choice of music has on their mental health.

library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

music_mh <- read.csv("mxmh_survey_results.csv")

head(music_mh)

str(music_mh)


#View the summary of the dataset
summary(music_mh)

#Data Cleaning
#Max. vs Quartile range of Age, 
#Hours.Per.Day and BPM shows the data has outliers.

# Drop rows where age is over 60

music_mh2 <- subset(music_mh, Age <= 60 )
# Drop rows where Hours.per.day is over 15

music_mh3 <- subset(music_mh2, Hours.per.day <= 15 )
# Drop rows where BPM is over 250

music_mh4 <- subset(music_mh3, BPM <= 250 )
#Timetsamp is the time the survey was submitted after filling.

#Permissions has one unique value ( I understand )

#drop both columns

# Drop columns Timestamp, Permissions

music_mh_v2 = subset(music_mh4, select = -c(Timestamp,Permissions) )
#save new data

music_mh_v2 %>%
  write.csv("music_mh_v2")


#Data Analysis and Visualization
#View the different streaming platforms and the count of users
#Create a dataframe for streaming platform

streaming_platform <- music_mh_v2%>%
  filter(Primary.streaming.service != "") %>%
  group_by(Primary.streaming.service)%>%
  summarize(users = n())%>%
  arrange(desc(users))

View(streaming_platform)

# Visualize streaming_platform

ggplot ((data = streaming_platform), aes(x = reorder(Primary.streaming.service, -users), y = users)) + geom_col(fill = 'purple') + labs(title = "Streaming Platforms by Popularity")+ xlab("Streaming Platforms") + theme(axis.text.x= element_text(size =10, angle = 90))

#Check Age distribution of data
ggplot(music_mh_v2, aes(x = Age)) + 
  geom_histogram(binwidth = 3, fill = "purple", colour = "black") + 
  labs(title = "Age distribution")

#Findings: Teenagers listen to music more frequently than other age groups.

#Analyze Genre Preference:

Genre <- music_mh_v2%>%
  group_by(Fav.genre)%>%
  summarize(number = n())%>%
  arrange(desc(number))


ggplot(Genre, aes(x = number, y = reorder(Fav.genre, number))) +
  geom_col(fill = 'purple') + labs(title = "Genre Preference") +
  ylab("Fav Genre")

#Findings: Rock is the most preferred genre of music, 
#while Gospel and latin are the least preferred music genres

# Check the age distribution of genre preference:

ggplot(data = music_mh_v2, aes(x=Age,y=Fav.genre)) + 
  geom_point(colour = "purple")

#finings: Rock music preference spans across all age groups,
#While genres like Rap, R&B, Lof and K Pop are preferred by younger
#individuals.


#How do individuals listen to music - while working or not

working <- music_mh_v2 %>%
  filter (While.working != "") %>%
  group_by (While.working)

ggplot(data=working, aes(x = While.working)) + 
  geom_bar( fill = 'purple')  + labs(title = 
                                       "Preference for listening to music while working")+ xlab("While working") +
  theme(axis.text.x= element_text(size =10))

#Findings:More people prefer to listen to music while they work.


#Plot Hours of music listened to daily

ggplot(music_mh_v2, aes(x = Hours.per.day)) +
  geom_histogram(binwidth = 1, fill = "purple",
                 colour = "black") + labs(title = 
                                            "Hours of Music listened to daily")

#Most people listen to music for 1 - 3 hours a day.


#Check if any relationship exists between Age and Hours of music listened to daily

ggplot(data = music_mh_v2, aes( x = Age, y = Hours.per.day)) + geom_smooth (method = 'lm', colour = "purple")

#`geom_smooth()` using formula 'y ~ x'

#Findings: geom_smooth() was used to identify the general underlying 
#pattern in the data, which shows that hours of music listened to daily 
#tends to decrease as age increases.

#Count the number of Instrumentalists and Composers

music_mh_v2 %>%
  filter (Instrumentalist != "") %>%
  count(Instrumentalist) %>%
  group_by (Instrumentalist)

music_mh_v2 %>%
  filter (Composer != "") %>%
  count(Composer) %>%
  group_by (Composer)


#Findings: There are more Instrumentalists than Composers.


#Analyze the various mental conditions

#Check the age distribution of people who experience each mental condition

#Depression:
ggplot(data = music_mh_v2, aes(x = Age, y = Depression)) + geom_smooth(colour = "purple") + labs(title = "Age distribition: Depression")

#Anxiety:
ggplot(data = music_mh_v2, aes(x = Age, y = Anxiety)) + geom_smooth(colour = "purple") + labs(title = "Age distribition: Anxiety")

#Insomnia:
ggplot(data = music_mh_v2, aes(x = Age, y = Insomnia)) + geom_smooth(colour = "purple") + labs(title = "Age distribition: Insomnia")

#OCD:
ggplot(data = music_mh_v2, aes(x = Age, y = OCD)) + geom_smooth(colour = "purple") + labs(title = "Age distribition: OCD")


#`geom_smooth()` using method = 'loess' and formula 'y ~ x'

#`geom_smooth()` using method = 'loess' and formula 'y ~ x'

#Findings:Teenagers experience higher levels of Anxiety, 
#Insomnia and OCD than older people. People in their twenties, 
#however, experience higher levels of Depression than other age groups.

#Analyze the effect of music on mental conditions

mh <- music_mh_v2 %>%
  filter (Music.effects != "") %>%
  group_by (Music.effects) %>%
  summarize(number =n())

View (mh)

ggplot(mh, aes(x= Music.effects, y = number)) + geom_col(fill = "purple") + 
  labs(title = "Music effect on Mental Condition")+ xlab("Music Effect") + theme(axis.text.x= element_text(size =10))


#Findings:75% of people indicated that their mental conditions improved 
#with music, while 3% reported that their mental conditions worsened with 
#music


#View the effect per condition
#Create subset for Anxiety

an <- subset(music_mh_v2, select = c(Fav.genre, Anxiety, Music.effects))%>%
  filter(Music.effects != "") %>%
  filter(Anxiety > 0)


ggplot(an, aes(x = Fav.genre, fill = Music.effects)) +
  geom_bar(position="dodge") + labs(title = "Music effect on Anxiety") + 
  xlab("Fav Genre") + theme(axis.text.x= element_text(size =10, angle = 90)) + 
  scale_fill_manual(breaks =c("Improve","No effect","Worsen"),values=c("Purple", "violet", "grey"))

#Findings: People with Anxiety who listened to Latin, Lofi and Gospel, reported improved conditions only.

#Create subset for Depression

de <- subset(music_mh_v2, select = c(Fav.genre, Depression, Music.effects))%>%
  filter(Music.effects != "")%>%
  filter(Depression > 0)
ggplot(de, aes(x = Fav.genre, fill = Music.effects)) + 
  geom_bar(position="dodge") + 
  labs(title = "Music effect on Depression") + xlab("Fav Genre") + 
  theme(axis.text.x= element_text(size =10, angle = 90)) + 
  scale_fill_manual(breaks =c("Improve","No effect","Worsen"),
                    values=c("Purple", "violet", "grey"))


#Findinggs:People with Depression who listened to Latin and Gospel,
#reported improved conditions only.

#Create subset for Insomnia

In <- subset(music_mh_v2, select = c(Fav.genre, Insomnia, Music.effects))%>%
  filter(Music.effects != "") %>%
  filter(Insomnia > 0)
ggplot(In, aes(x = Fav.genre, fill = Music.effects)) + 
  geom_bar(position="dodge") + labs(title = "Music effect on Insomnia") +
  xlab("Fav Genre") + theme(axis.text.x= element_text(size =10, angle = 90)) +
  scale_fill_manual(breaks =c("Improve","No effect","Worsen"),values=c("Purple", 
                                                                       "violet", "grey"))
#Findings: People with Insomnia who listened to Lofi and Gospel, 
#reported improved conditions only.


#Create subset for OCD

ocd <- subset(music_mh_v2, select = c(Fav.genre, OCD, Music.effects))%>%
  filter(Music.effects != "") %>%
  filter(OCD > 0)
ggplot(ocd, aes(x = Fav.genre, fill = Music.effects)) + geom_bar(position="dodge") + labs(title = "Music effect on OCD") + xlab("Fav Genre") + theme(axis.text.x= element_text(size =10, angle = 90)) + scale_fill_manual(breaks =c("Improve","No effect","Worsen"),values=c("Purple", "violet", "grey"))


#Findings: People with OCD who listened to Lofi and Gospel, 
#reported improved conditions only. Latin music had no effect on OCD.