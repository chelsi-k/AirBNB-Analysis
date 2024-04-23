install.packages("tidyverse")
install.packages("psych")
install.packages("GGally")
install.packages("leaps")


library(psych)
library(tidyverse)
library(GGally)
library(leaps)

# EDA
airbnb_df <- read.csv("data/AB_NYC_2019.csv")
# first we drop all extra features 
airbnb_df <- subset(airbnb_df, select=c(reviews_per_month, room_type, 
                                        latitude, longitude, price, neighbourhood_group,
                                        minimum_nights, availability_365, 
                                        calculated_host_listings_count))

# MERGING min_nights and price to make min_price
airbnb_df$minimum_price = airbnb_df$price*airbnb_df$minimum_nights
airbnb_df <- subset(airbnb_df, select=-c(price, minimum_nights))

# to check NA values in all columns
sapply(airbnb_df, function(x) sum(is.na(x)))

airbnb_df <- na.omit(airbnb_df)

ggpairs(subset(airbnb_df, select=-c(room_type)))

# reviews per month dist
hist(airbnb_df$reviews_per_month, xlab = "Reviews Per Month", main = "Distribution of Reviews Per Month")
hist(airbnb_df$calculated_host_listings_count, xlab = "Calculated Host Listings", main = "Distribution of Calculated Host Listings")
hist(airbnb_df$minimum_price, xlab = "Minimium Price", main = "Distribution of Minimum Price")

#removing outliers from cal_host listings count
cal_host_list_mean <- mean(airbnb_df$calculated_host_listings_count)
cal_host_list_sd <- sd(airbnb_df$calculated_host_listings_count)
airbnb_df <- subset(airbnb_df, calculated_host_listings_count <= cal_host_list_mean + 1.96*cal_host_list_sd)

#transformed histograms
hist(log(airbnb_df$reviews_per_month),xlab = "Transformed Reviews Per Month (log transformation)", main = "Distribution of Transformed Reviews Per Month")
hist(log(airbnb_df$minimum_price)^(1/3),xlab = "Transformed Minimum Price (log transformation)", main = "Distribution of Transformed Minimum Price")
hist(log(airbnb_df$calculated_host_listings_count)^(1/2),xlab = "Transformed Calculated host listings (log transformation)", main = "Distribution of Transformed Calculated host listings")