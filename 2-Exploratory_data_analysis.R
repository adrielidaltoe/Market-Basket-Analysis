#########################################################################################


    # Market Basket Analysis - Part 2 - Exploratory Data Analysis


#########################################################################################

setwd("D:/DataScienceAcademy/FCD/Projetos/Market_basket_analysis/Market-Basket-Analysis")

# Attribute Information:

# InvoiceNo: Number uniquely assigned to each transaction.
# StockCode: Product code. Number uniquely assigned to each distinct product.
# Description: Product (item) name.
# Quantity: The quantities of each product (item) per transaction. 
# InvoiceDate: The day and time when each transaction was generated.
# UnitPrice: Product price per unit in sterling.
# CustomerID: Number uniquely assigned to each customer.
# Country: Country where each customer resides.

# Import packages
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(treemap)
#library(arules)
#library(arulesViz)
library(tidyr)

# Import cleaned data
df <- fread('retail.csv')
View(df)
str(df)


df <- df %>% mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>%
  mutate(InvoiceNo = as.numeric(InvoiceNo)) %>%
  mutate(Date = as.Date(InvoiceDate)) %>%
  mutate(Time = as.factor(format(InvoiceDate, '%H:%M:%S')))

# What are the top 10 products in sales?

df %>% group_by(Description) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count*100/sum(count)) %>%
  arrange(desc(count)) %>%
  head(10) %>% 
  ggplot(aes(y = reorder(Description,percent), x = percent)) + 
  geom_bar(stat = 'identity', fill = "turquoise4") + 
  ggtitle('Top 10 products in sales') + ylab('') +
  xlab('% of sales')

# What are the top 10 products sold in large amount?
df %>% group_by(Description) %>% 
  summarise(count = sum(Quantity)) %>% 
  mutate(perct_count = (count/sum(count))*100) %>%
  arrange(desc(perct_count)) %>%
  head(10) %>% 
  ggplot(aes(y = reorder(Description,perct_count), x = perct_count)) + 
  geom_bar(stat = 'identity', fill = "turquoise") + 
  ggtitle('Top 10 products in Quantity') + ylab('') + 
  xlab('Quantity (%)')

# Quais os produtos mais lucrativos

df %>%
  group_by(Description) %>% 
  summarise(Quant_sold = sum(Quantity),
            Profit = sum(Quantity*UnitPrice)) %>%
  mutate(Profit_perc = (Profit/sum(Profit))*100) %>%
  arrange(desc(Profit_perc)) %>%
  head(10) %>%
  ggplot(aes(x = Profit_perc, y = reorder(Description,Profit_perc))) + 
  geom_bar(stat = 'identity', fill = "royalblue2") + 
  ggtitle('10 most profitable products') + ylab('') + xlab('Profit (%)')


# What time do peaple often purchase?

# O maior número de compras ocorre no intervalo de 10 às 16 horas, sendo o pico ao meio dia. 
ggplot(df, aes(x = lubridate::hour(hms(Time)))) + 
  geom_histogram(stat = 'count', fill="indianred") + 
  ggtitle('Shopping time distribution') + 
  scale_x_continuous(breaks = seq(6,20, by = 1)) + 
  labs(x = 'Hour of the day', y = '')

# What day of the week do people buy more often?
weekday <- as.data.frame(lubridate::wday(as.Date(df$Date)))
colnames(weekday) <- c('Weekday')

ggplot(weekday, aes(x = Weekday)) + geom_histogram(stat = 'count', fill = "sienna3") + 
  scale_x_continuous(breaks = c(1:7), 
                     labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# How many items is purchased by sale?
df %>% group_by(InvoiceNo) %>% summarise(n_items = n()) %>% 
  ggplot(aes(x = n_items)) + geom_histogram(stat = 'count', bins = 1000, colour = 'black') + 
 ggtitle('Number of different products in sales') + 
  labs(x='Number of products', y = 'Purchases') + coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10))

df %>% 
  group_by(InvoiceNo) %>% 
  summarise(n = mean(Quantity)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 100000, fill = "purple", colour = "black") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Number of Items (quantity) per Purchase", y = "")
  
# What is the average value per order?
df %>% mutate(Value = Quantity*UnitPrice) %>%
  group_by(InvoiceNo) %>%
  summarise(mean_value = mean(Value)) %>% 
  ggplot(aes(x=mean_value)) + geom_histogram(bins = 100000, fill = "tan4", colour = 'black') +
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = 'Average value per purchase')

# Which countries they sell their goods?
?treemap
treemap(df,
        index = c("Country"),
        vSize = "Quantity",
        title = "",
        palette = "Set3",
        border.col = "grey40")
RColorBrewer::display.brewer.all()

dim(df)

# Time series analysis

df %>% group_by(Date) %>% 
  summarise(purchases = n()) %>%
  ggplot(aes(x = Date, y = purchases)) +
  geom_line(color="#69b3a2") +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09"))) +
  ggtitle('Sales volume in time')


df %>% mutate(profit = Quantity*UnitPrice) %>% 
  group_by(Date) %>%
  summarise(profit_day = sum(profit)) %>%
  ggplot(aes(x = Date, y = profit_day)) +
  geom_line(color="#69b3a2") +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09"))) +
  ggtitle('Profit per day')

df %>% 
  group_by(Date) %>%
  summarise(quantity_day = sum(Quantity)) %>%
  ggplot(aes(x = Date, y = quantity_day)) +
  geom_line(color="#69b3a2") +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09"))) +
  ggtitle('Products saled per day')




