# Set the working directory and load libraries
setwd('C:/Users/fabri/OneDrive/Desktop/Courses/FA582 data science/First Assignment')
library(doBy)
library(ggplot2)

# Introduction

# This report analyzes data from the New York Times homepage. The dataset consists of three days of ads shown and clicks recorded. We will explore various aspects of the data and perform data analysis.

## Data Loading and Overview
data1 <- read.csv('nyt1.csv')
data2 <- read.csv('nyt2.csv')
data3 <- read.csv('nyt3.csv')

# Combine data into a single dataframe
all_data <- rbind(data1, data2, data3)

# Display the first and last few rows and summary statistics of the combined data
head(all_data)
tail(all_data)
summary(all_data)

dim(all_data)
length(all_data)

# Point 1 - Age Group Analysis

# Define age groups and calculate summary statistics for each group
data1$age_group <- cut(data1$Age, c(-Inf, 20, 29, 39, 49, 59, 69, Inf))
data2$age_group <- cut(data2$Age, c(-Inf, 20, 29, 39, 49, 59, 69, Inf))
data3$age_group <- cut(data3$Age, c(-Inf, 20, 29, 39, 49, 59, 69, Inf))
all_data$age_group <- cut(all_data$Age, c(-Inf, 20, 29, 39, 49, 59, 69, Inf))

siterange <- function(x){c(length(x), min(x), mean(x), max(x))}

summary_age1 <- summaryBy(Age ~ age_group, data = data1, FUN = siterange)
colnames(summary_age1) <- c("age_group", "length", "min", "mean", "max")
summary_age1

summary_age2 <- summaryBy(Age ~ age_group, data = data2, FUN = siterange)
colnames(summary_age2) <- c("age_group", "length", "min", "mean", "max")
summary_age2

summary_age3 <- summaryBy(Age ~ age_group, data = data3, FUN = siterange)
colnames(summary_age3) <- c("age_group", "length", "min", "mean", "max")
summary_age3

summary_age <- summaryBy(Age ~ age_group, data = all_data, FUN = siterange)
colnames(summary_age) <- c("age_group", "length", "min", "mean", "max")
summary_age

# We can observe that in all the age groups, the '0-20' age group consistently has the highest 'length' value


# Point 2
# Plot the distribution of Impressions by Age Group
ggplot(data1, aes(x = age_group, y = Impressions, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Distribution of Impressions by Age Group data 1",
       x = "Age Group",
       y = "Impressions")

ggplot(data2, aes(x = age_group, y = Impressions, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Distribution of Impressions by Age Group data 2",
       x = "Age Group",
       y = "Impressions")

ggplot(data3, aes(x = age_group, y = Impressions, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Distribution of Impressions by Age Group data 3",
       x = "Age Group",
       y = "Impressions")

#The maximum number of impressions ranges from 16 to 20 across different categories, while the average (mean) number of impressions for the various categories consistently hovers around 5

# Filter data for Impressions > 0 and plot CTR distribution by Age Group
ggplot(subset(data1, Impressions > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Impressions > 0",
       x = "CTR",
       y = "Density")

ggplot(subset(data2, Impressions > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Impressions > 0",
       x = "CTR",
       y = "Density")

ggplot(subset(data3, Impressions > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Impressions > 0",
       x = "CTR",
       y = "Density")

# From this plot, we can observe that the CTR (Click-Through Rate) distribution for Impressions greater than 0 in different age groups tends to be concentrated around 0. This indicates that, on average, there are relatively few clicks relative to the number of impressions, across various age categories. 


# Filter data for Clicks > 0 and plot CTR distribution by Age Group
ggplot(subset(data1, Clicks > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Clicks > 0",
       x = "CTR",
       y = "Density")

ggplot(subset(data2, Clicks > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Clicks > 0",
       x = "CTR",
       y = "Density")

ggplot(subset(data3, Clicks > 0), aes(x = Clicks/Impressions, colour = age_group)) +
  geom_density() +
  labs(title = "CTR Distribution for Clicks > 0",
       x = "CTR",
       y = "Density")

#From this plot, it appears that the first category, 0-20, has the highest exposure to advertisements among the different age groups. 


# Point 2.2
# Create a new variable to categorize users based on their click behavior
data1$scode[data1$Impressions==0] <- "No Imps" 
data1$scode[data1$Impressions >0] <- "Impr"    
data1$scode[data1$Clicks >0] <- "Clicks"

data2$scode[data2$Impressions==0] <- "No Imps" 
data2$scode[data2$Impressions >0] <- "Impr" 
data2$scode[data2$Clicks >0] <- "Clicks"

data3$scode[data3$Impressions==0] <- "No Imps" 
data3$scode[data3$Impressions >0] <- "Impr"    
data3$scode[data3$Clicks >0] <- "Clicks"

# Convert the column to a factor
data1$scode <- factor(data1$scode)
data1$Signed_In <- factor(data1$Signed_In, levels = c(0, 1), labels = c('No', 'Yes'))
data1$Gender <- factor(data1$Gender, levels = c(0, 1), labels = c('Female', 'Male'))

data2$scode <- factor(data2$scode)
data2$Signed_In <- factor(data2$Signed_In, levels = c(0, 1), labels = c('No', 'Yes'))
data2$Gender <- factor(data2$Gender, levels = c(0, 1), labels = c('Female', 'Male'))

data3$scode <- factor(data3$scode)
data3$Signed_In <- factor(data3$Signed_In, levels = c(0, 1), labels = c('No', 'Yes'))
data3$Gender <- factor(data3$Gender, levels = c(0, 1), labels = c('Female', 'Male'))

# Filter data for users with age < 20
subset_data1_age <- subset(data1, Age < 20)
subset_data2_age <- subset(data2, Age < 20)
subset_data3_age <- subset(data3, Age < 20)

# Create grouped bar plots to compare Gender and Logged-In Status for each day
ggplot(subset_data1_age, aes(x = Gender, fill = Signed_In)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender and Logged-In Status among Users with Age < 20 (Data1)",
       x = "Gender",
       y = "Population")

# Summary statistics for Gender and Logged-In Status among users with Age < 20
summary_data1_age <- summaryBy(Signed_In ~ Gender, data = subset_data1_age, FUN = siterange)
colnames(summary_data1_age) <- c("gender", "length", "min", "mean", "max")
summary_data1_age

ggplot(subset_data2_age, aes(x = Gender, fill = Signed_In)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender and Logged-In Status among Users with Age < 20 (Data2)",
       x = "Gender",
       y = "Population")

summary_data2_age <- summaryBy(Signed_In ~ Gender, data = subset_data2_age, FUN = siterange)
colnames(summary_data2_age) <- c("gender", "length", "min", "mean", "max")
summary_data2_age

ggplot(subset_data3_age, aes(x = Gender, fill = Signed_In)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender and Logged-In Status among Users with Age < 20 (Data3)",
       x = "Gender",
       y = "Population")

summary_data3_age <- summaryBy(Signed_In ~ Gender, data = subset_data3_age, FUN = siterange)
colnames(summary_data3_age) <- c("gender", "length", "min", "mean", "max")
summary_data3_age

# We can observe that there are a significant number of females who are not signed in, while all males in this category are signed in

# Create grouped bar plots to compare Gender and Age Group for each day
ggplot(data1, aes(x = Gender, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution among All Users (Data1)",
       x = "Gender",
       y = "Population")

# Summary statistics for Gender distribution among all users
summary_data1 <- summaryBy(age_group ~ Gender, data = data1, FUN = siterange)
colnames(summary_data1) <- c("gender", "length", "min", "mean", "max")
summary_data1

ggplot(data2, aes(x = Gender, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution among All Users (Data2)",
       x = "Gender",
       y = "Population")

summary_data2 <- summaryBy(age_group ~ Gender, data = data2, FUN = siterange)
colnames(summary_data2) <- c("gender", "length", "min", "mean", "max")
summary_data2

ggplot(data3, aes(x = Gender, fill = age_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution among All Users (Data3)",
       x = "Gender",
       y = "Population")

summary_data3 <- summaryBy(age_group ~ Gender, data = data3, FUN = siterange)
colnames(summary_data3) <- c("gender", "length", "min", "mean", "max")
summary_data3

# We can clearly see that the sample of females in the 0-20 age group is indeed substantial compared to the other categories

# Create grouped bar plots for gender distribution among users with Age < 20 for each day
ggplot(subset(data1, Age < 20), aes(x = Gender, fill = scode)) +
  geom_bar() +
  labs(title = "Gender Distribution among Users with Age < 20 (Data1)",
       x = "Gender",
       y = "Population")

# Summary statistics for Gender distribution among users with Age < 20
summary_by1 <- summaryBy(scode ~ Gender, data = data1, FUN = siterange)
colnames(summary_by1) <- c("gender", "length", "min", "mean", "max")
summary_by1 

ggplot(subset(data2, Age < 20), aes(x = Gender, fill = scode)) +
  geom_bar() +
  labs(title = "Gender Distribution among Users with Age < 20 (Data2)",
       x = "Gender",
       y = "Population")

summary_by2 <- summaryBy(scode ~ Gender, data = data2, FUN = siterange)
colnames(summary_by2) <- c("gender", "length", "min", "mean", "max")
summary_by2 

ggplot(subset(data3, Age < 20), aes(x = Gender, fill = scode)) +
  geom_bar() +
  labs(title = "Gender Distribution among Users with Age < 20 (Data3)",
       x = "Gender",
       y = "Population")

summary_by3 <- summaryBy(scode ~ Gender, data = data3, FUN = siterange)
colnames(summary_by3) <- c("gender", "length", "min", "mean", "max")
summary_by3 

# It's noteworthy that within this age group, there is a substantial number of impressions in contrast to the counts of clicks or non-impressions.

#Point 3

# Analysis across days
data1$weekday<-1
data2$weekday<-2
data3$weekday<-3
alldata<-rbind(data1,data2,data3)
alldata$weekday<-as.factor(alldata$weekday)

# Boxplot of Impressions by Signed_In and weekday
ggplot(alldata, aes(x = Signed_In, y = Impressions, color = weekday)) +
  geom_boxplot() +
  labs(title = "Number of Impressions by Signed_In and weekday",
       x = "Signed_In",
       y = "Impressions") +
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green"))

# Summary statistics for Impressions by Signed_In and weekday
summary_by_signed_in <- summaryBy(Impressions ~ Signed_In + weekday, data = alldata, FUN = siterange)
colnames(summary_by_signed_in) <- c("Signed_In", "Weekday", "Length", "Min", "Mean", "Max")
summary_by_signed_in

# It's evident that the impressions by signed-in users across different days consistently reach a peak at approximately 18-20, while the average remains stable at around 5

# Boxplot of 'Impressions' by Gender, colored by weekday
ggplot(alldata, aes(x = Gender, y = Impressions, fill = weekday)) +
  geom_boxplot() +
  labs(title = "Distribution of Impressions by Gender and Weekday",
       x = "Gender",
       y = "Impressions")

# Summary statistics for 'Impressions' by Gender and Weekday
summary_impressions_gender_weekday <- summaryBy(Impressions ~ Gender + weekday, data = alldata, FUN = siterange)
colnames(summary_impressions_gender_weekday) <- c("Gender", "Weekday", "Length", "Min", "Mean", "Max")
summary_impressions_gender_weekday

# We can observe that the maximum number of impressions varies between 17 and 20 on different days. This holds true for both male and female genders, with an average around 5.

ggplot(subset(alldata, Clicks > 0), aes(x = Clicks/Impressions, colour = weekday)) +
  geom_density() +
  labs(title = "CTR Distribution for Clicks > 0",
       x = "CTR",
       y = "Density") +
  theme_minimal()

# We can highlight there is more density around the 25%