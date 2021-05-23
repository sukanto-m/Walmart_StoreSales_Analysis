#Case Study to Analyse Walmart Store Sales
#Date Created: 23/5/2021
#Author: Sukanto Mukherjee

#Installing and Loading the Required Packages

getwd()
setwd('/users/sukanto/WD')
install.packages(c("ggplot2","lubridate","raster","zoo"))
my_packages <- c("ggplot2","lubridate","dplyr")
lapply(my_packages, require, character.only = TRUE)

#Loading the dataset and taking a preliminary look

stores <- read.csv('Walmart_Store_sales.csv')
head(stores)
summary(stores)
colnames(stores)
str(stores)

#The data column needs to be converted from character to date, days and 
# month format (new columns added to that end)

stores$Date <- as.Date(stores$Date, format = "%d-%m-%Y")
stores$Month <- month(stores$Date)
stores$Weekday <- weekdays(stores$Date)
stores$Weekday <- as.factor(stores$Weekday)
head(stores)

#Exploratory Data Analysis
# Which store had maximum sales? 
each_store <- aggregate(stores$Weekly_Sales, list(Store = stores$Store), sum)
max(each_store)
store_bar <- each_store$x
names(store_bar) <- each_store$Store
store_bar
jpeg(file = 'total_store_sales.jpg')
barplot(store_bar,
        main = 'Total Sales by Store',
        ylab = 'Store',
        xlab = 'Total Sales in Million $',
        col = 'darkblue',
        horiz = TRUE)
dev.off()

