# Set working directory
setwd('C:/Users/fabri/OneDrive/Desktop/Courses/FA582 data science/First Assignment')

# Load required libraries
library(openxlsx)
library(doBy)
library(ggplot2)

# Load data for different boroughs
data_bronx <- read.xlsx('rollingsales_bronx.xlsx', 1, startRow = 5)
data_manhattan <- read.xlsx('rollingsales_manhattan.xlsx', 1, startRow = 5)
data_brooklyn <- read.xlsx('rollingsales_brooklyn.xlsx', 1, startRow = 5)
data_queens <- read.xlsx('rollingsales_queens.xlsx', 1, startRow = 5)
data_statenisland <- read.xlsx('rollingsales_statenisland.xlsx', 1, startRow = 5)

# Combine data from all boroughs into one dataset
all_data <- rbind(data_bronx, data_manhattan, data_brooklyn, data_queens, data_statenisland)

# Display the first and last few rows of the combined dataset
head(all_data)
tail(all_data)

# Summarize the combined dataset
summary(all_data)

# Find unique values in the BUILDING.CLASS.CATEGORY column
unique(all_data$BUILDING.CLASS.CATEGORY)

# Change column names to lowercase
names(all_data) <- tolower(names(all_data))

# Convert the 'sale.date' column
all_data$sale.date <- convertToDateTime(as.numeric(all_data$sale.date))

# Summarize the sale price column
summary(all_data$sale.price)

# Count missing values in all columns of the all_data DataFrame
missing_values <- colSums(is.na(all_data))

# Display the count of missing values for each column
missing_values

# We can highlight there are not missing value

# Count values equal to 0 in all columns of the all_data DataFrame
zero_values <- colSums(all_data == 0, na.rm = TRUE)
zero_values

# Right now we can just observe different 0, we evaluate it later

# Calculate the proportion of values equal to 0 in each column
zero_proportion <- colSums(all_data == 0, na.rm = TRUE) / length(all_data [,1])
zero_proportion

# Set up data with regular expressions
all_data$borough<-as.factor(all_data$borough)
all_data$building.class.category<-as.factor(all_data$building.class.category)


# Exploratory data analysis
# Identify rows with sale price equal to 0
selection <- all_data[all_data$sale.price == 0, ]

# Display the dimensions of the selection
dim(selection)


# Keep only the actual sales
plot(all_data$gross.square.feet, all_data$sale.price,
     main="Scatterplot for All Data",
     xlab="Gross Square Feet",
     ylab="Sale Price")

plot(log(all_data$gross.square.feet), log(all_data$sale.price), 
     main="Log-Transformed Scatterplot",
     xlab="Log of Gross Square Feet", 
     ylab="Log of Sale Price")

# Create a new dataset containing only non-zero sale prices
all_data_sales <- all_data[all_data$sale.price != 0, ]

# Plot the relationship between gross square feet and sale price for actual sales
plot(all_data_sales$gross.square.feet, all_data_sales$sale.price,
     main="Scatterplot for Actual Sales",
     xlab="Gross Square Feet",
     ylab="Sale Price")

plot(log(all_data_sales$gross.square.feet), log(all_data_sales$sale.price),
     main="Log-Transformed Scatterplot for Actual Sales",
     xlab="Log of Gross Square Feet",
     ylab="Log of Sale Price")

# Check for outliers in the sale price column
hist(all_data$sale.price)
plot(all_data$sale.price)
boxplot(all_data$sale.price)
summary(all_data$sale.price)

# we can observe some outliers, even when the sale price is 0

# Check for gross square feet when sale price is equal to 0
gross_square_feet_zero_sale_price <- all_data$gross.square.feet[all_data$sale.price == 0]
hist(gross_square_feet_zero_sale_price)

# we are able to catch if there were not real sales

# Loook and remove outliers
all_data$outliers <- (log(all_data$sale.price) <= 10) + 0
all_data$outliers2 <- (log(all_data$gross.square.feet) <= 2) + 0

all_data <- all_data[which(all_data$outliers == 0 & all_data$outliers2 == 0), ]

# Summarize the cleaned dataset
summary(all_data)

# As we can see, we have a cleaner dataframe


# Plot the relationship between gross square feet and sale price for cleaned data
plot(log(all_data$gross.square.feet), log(all_data$sale.price),
     main="Log-Transformed Scatterplot (Outliers Removed)",
     xlab="Log of Gross Square Feet",
     ylab="Log of Sale Price")

# Create a subset of the data for residential properties (single-family homes, coops, and condos)
# based on the 'building.class.category' column using regular expressions
all_data.homes <- all_data[grepl('FAMILY|COOPS|CONDO', all_data$building.class.category),]

# Display the subset of residential properties
all_data.homes
unique(all_data.homes$building.class.category)

#exploratory data analysis: statistics and visual graphs

siterange <- function(x){c(length(x), min(x), mean(x), max(x))}


# Calculate summary statistics for sale.price by sale.datewindow
summary_stats <- summaryBy(sale.price ~ sale.date, data = all_data.homes, FUN = siterange)
colnames(summary_stats) <- c("sale.date", "length", "min", "mean", "max")
summary_stats

# Create a boxplot using ggplot2
library(ggplot2)

ggplot(all_data.homes, aes(x = sale.date, y = log(sale.price), fill = sale.date)) +
  geom_boxplot() +
  labs(
    title = "Log-Transformed Sale Price Boxplot by Sale Date",
    x = "Sale Date",
    y = "Log of Sale Price"
  )

# Calculate summary statistics for sale.price by building class and borough
summary_stats2 <- summaryBy(sale.price ~ building.class.category + borough, data = all_data.homes, FUN = siterange)
colnames(summary_stats2) <- c("building.class.category",'borough', "length", "min", "mean", "max")
summary_stats2

ggplot(all_data.homes, aes(x = building.class.category, y = log(sale.price), fill = borough)) +
  geom_boxplot() + 
  labs(
    title = "Log-Transformed Sale Price Distribution by Building Class and Borough", 
    x = "Building Class Category", 
    y = "Log of Sale Price" 
  ) + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_fill_brewer(palette = "Set3")

summary_stats3 <- summaryBy(sale.price~building.class.category+sale.date, data=all_data.homes, FUN=siterange)
colnames(summary_stats3) <- c("building.class.category",'sale.date', "length", "min", "mean", "max")
summary_stats3

ggplot(all_data.homes, aes(x = building.class.category, y = log(sale.price), fill = sale.date)) +
  geom_violin(trim = FALSE) + 
  labs(
    title = "Log-Transformed Sale Price Distribution by Building Class and Sale Date",
    x = "Building Class Category",
    y = "Log of Sale Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Create models and plots for each borough in all_data.homes.

# Brooklyn (borough==3)
brooklyn <- all_data.homes[all_data.homes$borough == 3,]
plot(log(brooklyn$gross.square.feet), log(brooklyn$sale.price))
model2 <- lm(log(brooklyn$sale.price) ~ log(brooklyn$gross.square.feet))
summary(model2)
abline(model2, col='red', lwd=2)

# Bronx (borough==2)
bronx <- all_data.homes[all_data.homes$borough == 2,]
plot(log(bronx$gross.square.feet), log(bronx$sale.price))
model3 <- lm(log(bronx$sale.price) ~ log(bronx$gross.square.feet))
summary(model3)
abline(model3, col='red', lwd=2)

# Manhattan (borough==1)
manhattan <- all_data.homes[all_data.homes$borough == 1,]
plot(log(manhattan$gross.square.feet), log(manhattan$sale.price))
model1 <- lm(log(manhattan$sale.price) ~ log(manhattan$gross.square.feet))
summary(model1)
abline(model1, col='red', lwd=2)

# Queens (borough==4)
queens <- all_data.homes[all_data.homes$borough == 4,]
plot(log(queens$gross.square.feet), log(queens$sale.price))
model4 <- lm(log(queens$sale.price) ~ log(queens$gross.square.feet))
summary(model4)
abline(model4, col='red', lwd=2)

# Staten Island (borough==5)
statenisland <- all_data.homes[all_data.homes$borough == 5,]
plot(log(statenisland$gross.square.feet), log(statenisland$sale.price))
model5 <- lm(log(statenisland$sale.price) ~ log(statenisland$gross.square.feet))
summary(model5)
abline(model5, col='red', lwd=2)

# Overall model for all data (without borough distinction)
model6 <- lm(log(all_data.homes$sale.price) ~ log(all_data.homes$gross.square.feet))
summary(model6)
plot(log(all_data.homes$gross.square.feet), log(all_data.homes$sale.price))
abline(model6, col='red', lwd=2)
