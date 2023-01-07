library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggsignif)
library(corrplot)
library(caret)
library(pROC)
library(rpart)
library(randomForest)
library(kknn)
library(rpart.plot)
library(ggthemes)
library(NbClust)

ds_row = read.csv("diabetes.csv", stringsAsFactors = FALSE, header = TRUE)

head(ds_row)

summary(ds_row)

str(ds_row)

#Inference:We can see some independents and the dependent are integer, 
#so converting them to numeric and dependent to factor for further analysis.

ds_row <- as.data.frame(apply(ds_row,2,function(ds_row) as.numeric(ds_row)))
ds_row <- mutate(ds_row, Outcome = as.factor(ds_row$Outcome))
str(ds_row)

sapply(ds_row, function(x) sum(is.na(x)))

#Inference:we can see that there are no missing values in the dataset.

# remove outliers
rmv_outliers = function(df, field){
  m = mean(df[,field])
  s = sd(df[,field])
  thrs = 3*s
  out = df[df[,field]<=(m+thrs) & df[,field]>=(m-thrs),]
  return(out)
}
ds<-ds_row
for (x in names(ds_row)[1:8]){
  ds = rmv_outliers(ds,x)
}
str(ds)

#visualisation
#Pregnancies
Pregnancies_Histogram_Plot <- ggplot(ds, aes(Pregnancies, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "Pregnancies Histogram Plot") +
  theme_tufte()

print(Pregnancies_Histogram_Plot)

compared_list = list(c('1', '0'))  

Pregnancies_Box_Plot <- ggplot(ds, aes(Outcome, Pregnancies,fill = Outcome))+
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "Pregnancies Box Plot") +
  theme_tufte()

print(Pregnancies_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(Pregnancies), median(Pregnancies))

#Glucose
Glucose_Histogram_Plot <- ggplot(ds, aes(Glucose, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "Glucose Histogram Plot") +
  theme_tufte()

print(Glucose_Histogram_Plot)

Glucose_Box_Plot <- ggplot(ds, aes(Outcome, Glucose, fill = Outcome)) + 
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "Glucose Box Plot") +
  theme_tufte()

print(Glucose_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(Glucose), median(Glucose))

#BloodPressure
BloodPressure_Histogram_Plot <- ggplot(ds, aes(BloodPressure, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "BloodPressure Histogram Plot") +
  theme_tufte()

print(BloodPressure_Histogram_Plot)

BloodPressure_Box_Plot <- ggplot(ds, aes(Outcome, BloodPressure, fill = Outcome)) +
  geom_boxplot() + 
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "BloodPressure Box Plot") +
  theme_tufte()

print(BloodPressure_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(BloodPressure), median(BloodPressure))


#SkinThickness
SkinThickness_Histogram_Plot <- ggplot(ds, aes(SkinThickness, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "SkinThickness Histogram Plot") +
  theme_tufte()

print(SkinThickness_Histogram_Plot)

SkinThickness_Box_Plot <- ggplot(ds, aes(Outcome, SkinThickness, fill = Outcome)) +
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "SkinThickness Box Plot") +
  theme_tufte()

print(SkinThickness_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(SkinThickness), median(SkinThickness))

#Insulin
Insulin_Histogram_Plot <- ggplot(ds, aes(Insulin, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "Insulin Histogram Plot") +
  theme_tufte()

print(Insulin_Histogram_Plot)

Insulin_Box_Plot <- ggplot(ds, aes(Outcome, Insulin, fill = Outcome)) + 
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "Insulin Box Plot") +
  theme_tufte()

print(Insulin_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(Insulin), median(Insulin))

#BMI 
BMI_Histogram_Plot <- ggplot(ds, aes(BMI, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "BMI  Histogram Plot") +
  theme_tufte()

print(BMI_Histogram_Plot)

BMI_Box_Plot <- ggplot(ds, aes(Outcome, BMI, fill = Outcome)) +
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "BMI  Box Plot") +
  theme_tufte()

print(BMI_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(BMI), median(BMI))

#DiabetesPedigreeFunction 
DiabetesPedigreeFunction_Histogram_Plot <- ggplot(ds, aes(DiabetesPedigreeFunction, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "DiabetesPedigreeFunction Histogram Plot") +
  theme_tufte()

print(DiabetesPedigreeFunction_Histogram_Plot)

DiabetesPedigreeFunction_Box_Plot <- ggplot(ds, aes(Outcome, DiabetesPedigreeFunction, fill = Outcome)) +
  geom_boxplot()+
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "DiabetesPedigreeFunction Box Plot") +
  theme_tufte()

print(DiabetesPedigreeFunction_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`

ds %>%
  group_by(Outcome) %>%
  summarize(mean(DiabetesPedigreeFunction), median(DiabetesPedigreeFunction))


#Age  
Age_Histogram_Plot <- ggplot(ds, aes(Age, fill = Outcome)) +
  geom_histogram(position = "dodge") +
  labs(title = "Age Histogram Plot") +
  theme_tufte()

print(Age_Histogram_Plot)

Age_Box_Plot <- ggplot(ds, aes(Outcome, Age, fill = Outcome)) +
  geom_boxplot() +
  geom_signif(comparisons = compared_list, test = t.test) +
  labs(title = "Age Box Plot") +
  theme_tufte()

print(Age_Box_Plot)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ds %>%
  group_by(Outcome) %>%
  summarize(mean(Age), median(Age))

#Inference:The mean of all these predictor variables of patients are
#greater than that of healthy people.

#Outcome:
ggplot(ds) +
  geom_bar(aes(Outcome, fill = Outcome)) +
  labs(title = "Outcome Barchart") +
  theme_tufte() +
  coord_polar()

#correlation
corrplot(cor(ds[,-9], method = "spearman"), type = "lower", addCoef.col = "black", diag = FALSE)

#Inference:The plot shows that there is a relatively strong relationship
#between Age and Pregnancies.

Pregnancies_Age <- ggplot(ds,aes(Age, Pregnancies, color = Outcome)) +
  geom_smooth(se=FALSE)+
  geom_point()+ 
  ggtitle("Pregnancies~Age")+
  facet_wrap(~Outcome)+
  theme_tufte()
print(Pregnancies_Age)
#`geom_smooth()` using method = 'loess' and formula 'y ~ x'

#Inference:The plot shows that the number of pregnancies increases 
#with age before the age of 45 while decline after 45.

#Correlation Test
res <- cor.test(netflix$Runtime, netflix$IMDB.Score, 
                method = "pearson")
res
## 
##  Pearson's product-moment correlation
## 
## data:  netflix$Runtime and netflix$IMDB.Score
## t = -0.98744, df = 582, p-value = 0.3238
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.12162699  0.04037194
## sample estimates:
##         cor 
## -0.04089629
P-value
res$p.value
## [1] 0.3238393
##Correlation Coefficient
res$estimate
##         cor 
## -0.04089629