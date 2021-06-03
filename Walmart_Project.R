#Case Study to Analyse Walmart Store Sales
#Date Created: 23/5/2021
#Author: Sukanto Mukherjee

#Installing and Loading the Required Packages

getwd()
setwd('/users/sukanto/WD/Walmart_StoreSale_Analysis')
getwd()

#Installing required packages

install.packages(c("ggplot2","lubridate","raster","zoo","sp","usdm","lmtest"))
my_packages <- c("ggplot2","lubridate","dplyr", "ggplot2","lubridate","raster","zoo","sp","usdm","lmtest")
lapply(my_packages, require, character.only = TRUE)

#Loading the dataset and taking a preliminary look

stores <- read.csv('Walmart_Store_sales.csv')
head(stores)
summary(stores)
colnames(stores)
str(stores)

#checking for missing and duplicate data

sum(is.na(stores))
duplicated(stores)

#The data column needs to be converted from character to date, days and 
# month format (new columns added to that end)

stores$Date <- as.Date(stores$Date, format = "%d-%m-%Y")
stores$Month <- month(stores$Date)
stores$Weekday <- weekdays(stores$Date)
stores$Weekday <- as.factor(stores$Weekday)
stores$Quarter <- quarter(stores$Date, with_year = TRUE)
stores$Semester <- semester(stores$Date, with_year = TRUE)
head(stores)

#Exploratory Data Analysis
# Which store had maximum sales? 
each_store <- aggregate(stores$Weekly_Sales, list(Store = stores$Store), sum)
max(each_store)
store_bar <- each_store$x
names(store_bar) <- each_store$Store
store_bar
options(scipen = 999)
jpeg('total_store_sales.jpg')
barplot(store_bar,
        main = 'Total Sales by Store',
        ylab = 'Store',
        xlab = 'Total Sales in Million $',
        col = 'darkblue',
        horiz = TRUE)
dev.off()
#The sales vary a lot by store, checking which store has the maximum standard deviation

each_store_sd <- aggregate(stores$Weekly_Sales, list(Store = stores$Store), sd)
max(each_store_sd$x)

#Which store had good quarterly growth rate in Q3'2012?

store_growth <-stores[,c("Weekly_Sales","Quarter","Store")]
head(store_growth)
str(store_growth)
store_growth_q2 <- filter(store_growth, Quarter == 2012.2)
q2 <- aggregate(store_growth_q2$Weekly_Sales, 
                list(store_growth_q2$Store), sum)
store_growth_q3 <- filter(store_growth, Quarter == 2012.3)
q3 <- aggregate(store_growth_q3$Weekly_Sales, 
                list(store_growth_q3$Store),sum)
growth <- as.data.frame(q3$x - q2$x)
names(growth)[names(growth)=="q3$x - q2$x"] <- "growth"
growth
growth$Store <- seq.int(nrow(growth))
summary(growth)
growth <- growth[,order(ncol(growth):1)]
jpeg("Store_salesgrowth_Q2Q3.jpg")
barplot(growth$growth,
        main = "Store sales growth in Q2 vs Q32012",
        xlab = "Store",
        ylab = "Sales in Million $",
        col = "darkred")
dev.off()
growth_good <- filter(growth, growth > 0)
q3$Q2Sales <- q2$x
names(q3) <- c("Store","Q3Sales","Q2Sales")
barplot(height = q3$Q2Sales,q3$Q3Sales,
        names = q3$Store,
        main="Q2vsQ3 Store Sales",
        xlab="Store",
        ylab="Sales in Million $",
        col = c("blue","black"))

#Only 10 stores had maximum growth for said period, whereas most stores' growth declined in Q3

#Inspecting if some holiday weeks had a negative impact on sales


#holiday_sales <- filter(stores, Holiday_Flag == 1)
#head(holiday_sales)


#str(holiday_sales)
#holiday_sales$Year <- year(holiday_sales$Date)
#by_date <- holiday_sales %>% group_by(Date)
#hol_date <- by_date %>% summarise(sales = mean(Weekly_Sales))

#non_hol_sales <- filter(stores, Holiday_Flag == 0)
#head(non_hol_sales)
#by_date_non <- non_hol_sales %>% group_by(Date)
#non_sales <- by_date_non %>% summarise(sales = mean(Weekly_Sales))

hol_dates <- as.Date(c("2010-02-12","2011-02-11","2012-02-10","2013-02-08",
                 "2010-09-10","2011-09-11","2012-09-07","2013-09-06",
                 "2010-11-26","2011-11-25","2012-11-23","2013-11-29",
                 "2010-12-31","2011-12-30","2012-12-28","2013-12-27"))

holidays <- stores[stores$Date %in% hol_dates, ]
by_hol_date <- holidays %>% group_by(Date)
by_hol_date <- by_hol_date %>% summarise(sales = mean(Weekly_Sales))

no_hol_dates <- as.Date(c("2010-02-05","2011-02-04","2012-02-03","2013-02-01",
                       "2010-09-03","2011-09-04","2012-08-31","2013-08-30",
                       "2010-11-19","2011-11-18","2012-11-16","2013-11-22",
                       "2010-12-24","2011-12-23","2012-12-21","2013-12-20"))

no_holidays <- stores[stores$Date %in% no_hol_dates,]
by_date <- no_holidays %>% group_by(Date)
by_date <- by_date %>% summarise(sales = mean(Weekly_Sales))
class(by_date$Date)
plot(by_date$Date, by_date$sales, type = 'l')
install.packages('xts')
library(xts)
?xts

by_date$Date <- factor(by_date$Date)
ggplot(by_date, aes(Date, sales, group = 1)) + geom_line()

##hol_sales <- aggregate(holiday_sales$Weekly_Sales, list(Store = holiday_sales$Store), mean)

#Linear Regression to test which variables in the dataset affect sales for store 1

#Month and quarter to be converted to dummy variables
# Month - 1 for even numbered month, 0 for odd
# Quarter - 1 for even quarter, 0 for odd

store1 <- filter(stores, Store == '1')
Even_Month <- ifelse(store1$Month == 2 | store1$Month == 4 | store1$Month == 6 |
                store1$Month == 8 | store1$Month == 10 | store1$Month == 12, 
                1, 0)
Odd_Month <- ifelse(store1$Month == 1 | store1$Month == 3 | store1$Month == 5 |
                store1$Month == 7 | store1$Month == 9 | store1$Month == 11, 
                    1, 0)
store1$Quarter <- quarter(store1$Dat, with_year = FALSE)
store1$Semester <- semester(store1$Date, with_year = FALSE)
Even_Quarter <- ifelse(store1$Quarter == 2 | store1$Quarter == 4 | store1$Quarter == 6 |
                             store1$Quarter == 8 | store1$Quarter == 10 | store1$Quarter == 12, 
                     1, 0)
Odd_Quarter <- ifelse(store1$Quarter == 1 | store1$Quarter == 3 | store1$Quarter == 5 |
                            store1$Quarter == 7 | store1$Quarter == 9 | store1$Quarter == 11, 
                    1, 0)
store1$Even_Month <- Even_Month
store1$Odd_Month <- Odd_Month
store1$Even_Quarter <- Even_Quarter
store1$Odd_Quarter <- Odd_Quarter
store1 <- store1[-9,-10]
summary(store1)

str(store1)
plot(store1)


#Checking initial assumptions about holiday periods impacting sales


fit <- lm(Weekly_Sales~Holiday_Flag+Temperature+Fuel_Price+CPI+Unemployment, 
          data = store1)
fit
a <- coef(fit[1])
a
summary(fit)
plot(fit)

fit1 <- lm(Weekly_Sales ~ Temperature + CPI + Holiday_Flag, data = store1)
fit1
summary(fit1)
plot(fit1)
predict(fit1)
