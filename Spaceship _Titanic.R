library(tidyverse) # metapackage of all tidyverse packages

#Installing required packages:
install.packages('caret')
library(caret)

install.packages('mice')
library(mice)

install.packages('grindExtra')
library(gridExtra)

df = read_csv("train.csv")


#Exploratory Data Analysis:
#Look at data:
head(df)

# function to plot ggplot:
plot_gg = function(column){
  ggplot(data = df, mapping = aes(x = {{column}}, fill = Transported)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('legend', values = c("#F8766D", "#39954E"))
}

#Findings:HomePlanet definitely seems to be correlated 
#with Transported.Notably, the distribution of Transported values 
#in the NA data is much closer to data labeled Mars compared 
#to Earth.

# Cryosleep- Home planet:
plot_gg(HomePlanet) + 
  ggtitle("Transported Counts by HomePlanet")

# cryosleep
plot_gg(CryoSleep) + 
  ggtitle("Transported Counts by CryoSleep")

#Findings: It makes intuitive sense that the CryoSleep column is seemingly 
#strongly correlated with Transported. I'd like my chances of 
#staying on the ship more if I was conscious when the crash happened.

# destination:
plot_gg(Destination) + 
  ggtitle("Transported Counts by Destination")

#Findings: Destination looks to be another important column.
#The proportion of passengers Transported differs significantly
#depending on their Destination.

#Amenities (Total Spent):
# money spent total
df$total_spent = rowSums(df[c(8:12)], na.rm = TRUE)
df$total_spent_cat = ifelse(df$total_spent == 0, 1,
                            ifelse(df$total_spent < 1000, 2,
                                   ifelse(df$total_spent < 2000, 3, 4)))


# plot categorical var
plot_gg(total_spent_cat) + 
  ggtitle("Transported Counts by Total Spent")

# remove total spent
df = df %>% select(-c(total_spent, total_spent_cat))

#Findings: Spending on amenities is correlated with Transported. 

#VIP:
# plot VIP
plot_gg(VIP) + 
  ggtitle("Transported Counts by VIP")

#Findings:VIP doesn't seem to be too useful due to the similar
#proportions of Transported and the small amount of TRUE observations.


#Feature Engineering - There's multiple features that can be extracted
#from the Cabin and PassengerId columns. From looking at the data, you 
#can see that the last part of the PassengerId column represents the
#number of the person in their group. I extract that data to create a 
#group size column called size. Because there are such small amounts of 
#large groups, I decide to count groups of four or larger simply as four.


# split up cabin and passengerid columns
df2 = df %>% 
  mutate(deck = str_sub(Cabin, 1, 1),
         num = as.numeric(str_sub(Cabin, 3, -3)),
         side = str_sub(Cabin, -1, -1),
         id1 = substring(PassengerId, 1, 4),
         id2 = substring(PassengerId, 6, 7)
  )

# calculate group sizes
group_size = df2 %>% 
  select(id1, id2) %>% 
  group_by(id1) %>% 
  summarize(size = max(as.numeric(id2))) %>% # assign highest number in group to id
  select(id1, size)

# add feature to df2
df2 = merge(x = df2, y = group_size, by = "id1") %>% 
  mutate(size = ifelse(size >= 4, 4, size)) # max out the feature at 4
rm(group_size) # remove unnecessary group_size data

# plot the group size feature:
ggplot(data = df2, mapping = aes(x = size, fill = Transported)) +
  geom_bar(position = 'dodge') +
  ggtitle("Transported by Group Size") +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E"))

#Findings:It looks like creating the group size column was a smart move.
#Singles were disproportinately likely to stay on the ship.The first part 
#of PassengerId could contain useful information. It might reflect booking order.

# is there any useful information in the passengerid?

plot(1, type = 'n', xlim = c(-1000, 10000), ylim = c(0, 0.00014), 
     main = "PassengerId // Transported Distributions")
polygon(density(as.numeric(df2$id1[df2$Transported == 1])), col = alpha(3, 0.5))
polygon(density(as.numeric(df2$id1[df2$Transported == 0])), col = alpha(2, 0.5))
legend(x = 8800, y = .00014, c("FALSE", "TRUE"), box.lty = 0, 
       col = c(alpha(2, 0.5), alpha(3, 0.5)), pch = 20)

# plot cabin:
cabin1 = ggplot(data = df2, mapping = aes(x = deck, fill = Transported)) +
  geom_bar(position = 'dodge') +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E")) +
  ggtitle('Transported by Deck')
cabin2 = ggplot(data = df2, mapping = aes(x = side, fill = Transported)) +
  geom_bar(position = 'dodge') +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E")) +
  ggtitle("Transported by Side")
grid.arrange(cabin1, cabin2)

#Extracting deck and side from Cabin is an easy way to add useful 
#data to the model. There's clear heterogeneity among the decks and sides.

#Before continuing, I remove the Name, num, and Cabin features. 
#I looked at the Name column and decided that it's probably the 
#not worth the trouble to try and extract any value from it. 
#There doesn't seem to be any way to extract gender from it. 


# Let's assess how much data is missing.
# remove unwanted features
df3 = df2 %>% 
  select(-c(Name, Cabin))

# complete the dataset

# function to check proportion of missing values
na_prop = function(vec){
  sum(is.na(vec)) / length(vec)
}

# check props
lapply(df2[,1:ncol(df3)], na_prop)

#Findings:Most of the columns are missing about 2% of the data. That doesn't seem
#too bad. Let's see what proportion of rows are missing any values.

# check proportion of rows missing data
1 - (nrow(drop_na(df2)) / nrow(df2))

#Findings: 24% of the records have missing data, so simply getting rid of the rows 
#with missing data is not a viable option. We'll have to impute.


# CryoSleep-

#First, we know that passengers who are in CryoSleep won't be able to spend money at any of the amenities. That's a freebie.
# this is a freebie: if CryoSleep = TRUE, then $$ spent = 0


df3 = df3 %>%
  mutate(RoomService = ifelse(is.na(RoomService) & CryoSleep == TRUE, 0, RoomService),
         FoodCourt = ifelse(is.na(FoodCourt) & CryoSleep == TRUE, 0, FoodCourt),
         ShoppingMall = ifelse(is.na(ShoppingMall) & CryoSleep == TRUE, 0, ShoppingMall),
         Spa = ifelse(is.na(Spa) & CryoSleep == TRUE, 0, Spa),
         VRDeck = ifelse(is.na(VRDeck) & CryoSleep == TRUE, 0, VRDeck))

# check na_prop
lapply(df3 %>% select(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck), na_prop)

# That got rid of a good chunk of missing values for the amenities.


#Imputation Based on Group

#I figure that we can use the PassengerId data to fill in some values based on group. 
#If two people are traveling together, I'd bet they came from the same HomePlanet, are 
#going to the same Destination, and are more than likely staying on the same deck.

# see how many we can fix with home planet
df3_missing_hp = df3 %>% 
  select(HomePlanet, id1) %>% 
  filter(id1 %in% df3$id1[is.na(df3$HomePlanet)] & !is.na(HomePlanet)) %>% 
  unique()

# merge
df3 = left_join(df3, df3_missing_hp, by = "id1") %>%
  mutate(HomePlanet = ifelse(!is.na(HomePlanet.x), HomePlanet.x, ifelse(is.na(HomePlanet.y), NA, HomePlanet.y))) %>%     
  select(-c(HomePlanet.x, HomePlanet.y))

# same process for destination
df3_missing_dest = df3 %>% 
  select(Destination, id1) %>% 
  filter(id1 %in% df3$id1[is.na(df3$Destination)] & !is.na(Destination))  %>% 
  group_by(id1) %>%
  mutate(Destination = min(Destination)) %>% 
  unique()

# merge
df3 = left_join(df3, df3_missing_dest, by = "id1") %>%
  mutate(Destination = ifelse(!is.na(Destination.x), Destination.x, 
                              ifelse(is.na(Destination.y), NA, Destination.y))) %>%
  select(-c(Destination.x, Destination.y))

# same process for deck
df3_missing_deck = df3 %>% 
  select(deck, id1) %>% 
  filter(id1 %in% df3$id1[is.na(df3$deck)] & !is.na(deck)) %>%  
  group_by(id1) %>%
  mutate(deck = min(deck)) %>% 
  unique()

# merge
df3 = left_join(df3, df3_missing_deck, by = "id1") %>%
  mutate(deck = ifelse(!is.na(deck.x), deck.x, ifelse(is.na(deck.y), NA, deck.y))) %>%
  select(-c(deck.x, deck.y))

# check props
lapply(df3 %>% select(HomePlanet, Destination, deck), na_prop)

#Findings: This corrects ~ 50% of the missing values for these features.

##Lazy Imputation- 

#HomePlanet: considering the chart from earlier, fill in NAs with "Mars"
#deck, side, CryoSleep, Destination, VIP: fill in NAs with the mode
#num: to my surprise, leaving the second part of Cabin in the dataset 
#improves my models' performances. I'm lazily replacing missing values with
#the median, but it could be worth taking a deeper look into this

# function to find the mode


find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
  
  # lazy fill in
  df3 = df3 %>% 
    mutate(HomePlanet = ifelse(is.na(HomePlanet), "Mars", HomePlanet), # distribution match
           deck = ifelse(is.na(deck), find_mode(df2$deck), deck),
           side = ifelse(is.na(side), find_mode(df2$side), side),
           num = ifelse(is.na(num), median(df2$num, na.rm = TRUE), num),
           CryoSleep = ifelse(is.na(CryoSleep), find_mode(df2$CryoSleep), CryoSleep),
           Destination = ifelse(is.na(Destination), find_mode(df2$Destination), Destination),
           VIP = ifelse(is.na(VIP), find_mode(df2$VIP), VIP),
           Transported = as.factor(as.numeric(Transported))
    )
}

# remove unneccesary data:
rm(df3_missing_deck, df3_missing_dest, df3_missing_hp)

## Linear Model to Predict Age

# fix df3 feature types
df3 = df3 %>% 
  mutate(id1 = as.numeric(id1),
         id2 = as.numeric(id2),
         side = as.factor(side),
         HomePlanet = as.factor(HomePlanet),
         Destination = as.factor(Destination),
         deck = as.factor(deck))


# rmse function
rmse = function(model){
  preds = predict(model, test_age)
  sqrt(mean((preds - test_age$Age) ^ 2, na.rm = TRUE))
}


# create data frame without missing values

age_df = df3 %>% filter(!is.na(Age))
na_age_df = df3 %>% filter(is.na(Age))

# test // train
set.seed(1867)
trn_idx_age = sample(1:nrow(age_df), 0.8 * nrow(age_df))
train_age = age_df[trn_idx_age,] %>% select(-PassengerId)
test_age = age_df[-trn_idx_age,] %>% select(-PassengerId)

# linear fit
age_lm1 = lm(Age ~ ., train_age)

# check model
summary(age_lm1)
rmse(age_lm1)

lm(formula = Age ~ ., data = train_age)

head(df3)

#Findings:The id1, CryoSleep, id2, and side features weren't significant in our first model. 
#The amenities don't seem to add much predictive power.

#Let's try another model without the insignificant features and see how its RMSE compares to
#our first model's: 13.57.

# linear fit 2
age_lm2 = lm(Age ~ VIP + size + HomePlanet + Destination + deck, data = train_age)

# check model
summary(age_lm2)
rmse(age_lm2)


# what's our rmse if we did the lazy median guess?
sqrt(mean((train_age$Age - median(train_age$Age)) ^ 2))
Call:
  lm(formula = Age ~ VIP + size + HomePlanet + Destination + deck, 
     data = train_age)


#The model with less features is slightly better than the larger model with an RMSE
#of 13.47. This is notably better than simply using the median values, which gives us 
#an RMSE of 14.63. We'll retrain the model using all the data available, and then replace 
#the missing values with its predictions.


# train on entire dataset
age_lm2 = lm(Age ~ VIP + size + HomePlanet + Destination + deck, data = age_df)

# input new age predictions
df3$Age[is.na(df3$Age)] = predict(age_lm2, na_age_df)

#Multiple Imputation by Chain Equations (MICE) for Amenities

#It seems like filling in missing amenity values like this might be a bit 
#overkill, but I'm greedy and want to get some experience using MICE anyways.

# mice for amenities
amenity_df = df3 %>% 
  select(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck, VIP, Age, side, deck, size)

# impute values
amenity_mice = mice(amenity_df, method = c(rep("pmm", 5), rep("", 5)), maxit = 20)

# check which dataset you want to use
summary(amenity_df$RoomService)
amenity_mice$imp$RoomService

# impute new data
final_clean_amenity_df = complete(amenity_mice, 1)

# replace in full df
df3[,6:10] = final_clean_amenity_df[,1:5]

