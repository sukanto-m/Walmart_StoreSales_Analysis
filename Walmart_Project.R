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

#Store 45 has maximum growth, also most stores' growth declined in Q3


