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
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(treemap)
library(tidyr)
library(RColorBrewer)

# Import cleaned data
df <- read_csv('retail_clean.csv')
View(df)
dim(df)


# Creating new features
df <- df %>% mutate(InvoiceDate = as.POSIXct(InvoiceDate, format = "%Y-%m-%d %H:%M:%S"),
                    date = as.Date(df$InvoiceDate),
                    hour = hour(InvoiceDate),
                    weekday = wday(date))
str(df)

# What are the top 10 products that commonly appear in the transactions?

df %>% group_by(Description) %>% 
  summarise(count = n()) %>% 
  mutate(support = count/length(unique(df$InvoiceNo))) %>%
  arrange(desc(count)) %>%
  head(12) %>% 
  ggplot(aes(y = reorder(Description,support), x = support, fill = Description)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette="Paired") + 
  ggtitle('Produtos mais frequentes nas transações') + ylab('') +
  xlab('Suporte') +
  geom_text(aes(label = signif(support, digits = 3)), nudge_x = -0.005, color = 'black') +
  scale_x_continuous(breaks=c(0,0.03,0.06,0.09,0.11)) +
  theme(legend.position = "none") 
brewer.pal(12, 'Paired')
display.brewer.pal(12, 'Paired')

# How many products there were in each transaction?

freq_products <- df %>%
  group_by(InvoiceNo) %>%
  count() %>%
  group_by(n) %>%
  summarise(freq = n())

g1 <- freq_products %>%
ggplot(aes(x = n, y = freq)) +
  geom_point(color = "#33A02C") +
  scale_y_continuous(breaks=c(1,250,500,750,1000,1250,1505)) +
  scale_x_continuous(breaks=c(1,c(1:10)*100,1106)) +
  ylab('Transações') +
  xlab('Quantidade de Produtos') +
  ggtitle('Quantidade de produtos por transação')
freq_products %>% filter(n>100)
 
g2 <- freq_products %>%
  mutate(group = cut(x = n, breaks = c(0,10,50,100,1106), labels = c('>=10','>=50','>=100','>=1106'))) %>%
  group_by(group) %>%
  summarise(count = sum(freq)) %>%
  mutate(rel_freq = count/sum(count)) %>%
  mutate(cumul_sum = cumsum(rel_freq)) %>%
  ggplot(aes(x = group, y = cumul_sum)) +
  geom_line(color = "#E31A1C",group=1, size = 1) +
  geom_point(color = "#E31A1C", size=1.5) +
  xlab('Quantidade de produtos') +
  ylab('Transações') +
  ggtitle('Frequência acumulada') +
  theme(plot.title = element_text(size=10,face="bold"), axis.title=element_text(size=8))

g1 + annotation_custom(ggplotGrob(g2), xmin = 650, xmax = 1150, 
                  ymin = 250, ymax = 1500)


# What are the products that most contribute to the company revenue?

# most frequent products
df_products <- df %>% group_by(Description) %>% 
  summarise(count = n()) %>% 
  mutate(support = count/length(unique(df$InvoiceNo))) %>%
  arrange(desc(count)) %>%
  head(10)

prof_products <- df %>%
  group_by(Description) %>% 
  summarise(Quant_sold = sum(Quantity),
            Revenue = sum(Quantity*UnitPrice)) %>%
  mutate(Revenue_perc = (Revenue/sum(Revenue))*100) %>%
  arrange(desc(Revenue_perc)) %>%
  head(10) %>%
  mutate(toHighlight = ifelse(Description %in% df_products$Description, '1','0'))

prof_products %>%
  ggplot(aes(x = Revenue_perc, y = reorder(Description,Revenue_perc), fill = toHighlight)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Produtos com maior receita') + ylab('') + xlab('Receita total (%)') +
  scale_fill_manual(values = c('1'="#6A3D9A", '0'="gray" ), guide = FALSE ) +
  theme(legend.position = "none") 


# What are the top 10 products sold in quantity?

quant_products <- df %>% group_by(Description) %>% 
  summarise(count = sum(Quantity)) %>% 
  mutate(perct_count = count*100/sum(count)) %>%
  arrange(desc(perct_count)) %>%
  head(10) %>% 
  mutate(toHighlight = ifelse(Description %in% prof_products$Description, '1','0')) 
  
quant_products %>% 
  ggplot(aes(y = reorder(Description,perct_count), x = perct_count, fill = toHighlight)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('1'="#33A02C", '0'="gray" ), guide = FALSE ) +
  ggtitle('Produtos vendidos em maior quantidade') + ylab('') + 
  xlab('Quantidade (%)')+
  theme(legend.position = "none")


# Price of the most sold products in quantity.
df %>% distinct(Description,.keep_all = TRUE) %>%
  filter(Description %in% quant_products$Description) %>%
  select(Description, UnitPrice) %>%
  inner_join(quant_products, by = 'Description') %>%
  arrange(desc(perct_count)) %>%
  ggplot(aes(x = UnitPrice, y = reorder(Description,UnitPrice))) +
  geom_point(size =2,color='#33A02C') +
  ggtitle('Preço dos produtos vendidos em maior quantidade') +
  xlab('Preço (libra esterlina)') +
  ylab('')


# Quantity sold x UnitPrice
unit_price <- df %>% distinct(Description,.keep_all = TRUE) %>%
  select(Description, UnitPrice)

prod_count <- df %>% group_by(Description) %>% 
  summarise(count = sum(Quantity))

prod_count %>% inner_join(unit_price, by='Description') %>%
  ggplot(aes(x = log(1+UnitPrice), y= log(count))) +
  geom_point(color = '#FF7F00') +
  xlab('log(Preço)') +
  ylab('log(Quantidade)') +
  ggtitle('Relação entre Quantidade vendida e Preço')


# Is there a relationship between the most popular products in the transactions and the price?
support <- df %>% group_by(Description) %>% 
  summarise(count = n()) %>% 
  mutate(support = count/length(unique(df$InvoiceNo)))

most_popular <- support %>% arrange(desc(support)) %>% head(12)

colors <- c('1'="#96613d", '0'="#d2a56d")
support %>% inner_join(unit_price, by='Description') %>%
  arrange(desc(support)) %>%
  mutate(toHighlight = ifelse(Description %in% most_popular$Description,'Produtos mais populares (Figura 1)','Outros')) %>%
  ggplot(aes(x=log(1+UnitPrice), y = support)) +
  geom_point(aes(colour = toHighlight))+
  scale_color_manual(values = c("#FDBF6F", "#B15928"))+
  theme(legend.position = c(0.85,0.9), legend.background = element_blank())+
  labs(x = 'log(Preço)', y= 'Suporte',colour = '') +
  ggtitle('Relação entre Suporte e Preço')


# What time do peaple often purchase?
# What day of the week do people buy more often?

g3 <- df %>% 
  distinct(InvoiceNo, .keep_all = TRUE) %>%
  group_by(weekday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday,y=count)) +
  geom_bar(stat = 'identity',fill = "#1F78B4") + 
  scale_x_continuous(breaks = c(1:7), 
                      labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")) +
  xlab('') +
  ylab('') +
  ggtitle('Vendas por dia da semana')
  

# Hour range within 10 to 15 had the most amount of sales and 12h was the sales peak.
g4 <- df %>%
  group_by(InvoiceNo)%>%
  distinct(hour) %>%
  ggplot(aes(x = hour)) + 
  geom_histogram(stat = 'count', fill="#1F78B4") + 
  ggtitle('Vendas por horário do dia') + 
  scale_x_continuous(breaks = seq(6,20, by = 1)) + 
  labs(x = 'Hora', y = '')

gridExtra::grid.arrange(g3, g4, ncol = 2)
  
# What is the average price per order?
df %>% mutate(Value = Quantity*UnitPrice) %>%
  group_by(InvoiceNo) %>%
  summarise(mean_value = mean(Value)) %>% 
  ggplot(aes(x=mean_value)) + geom_histogram(bins = 25000, fill = "tan4", colour = 'black') +
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = 'Average value per purchase')

# Which countries they sell their goods?
df_treemap <- df %>% distinct(InvoiceNo, .keep_all = TRUE) %>%
  group_by(Country) %>%
  count()

treemap(df_treemap,
        index = c("Country",'n'),
        vSize = "n",
        title = "",
        palette = "Paired",
        border.col = "grey40",
        fontsize.labels=c(12,10),   # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontface.labels=c(2,1),     # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"), # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c('right', 'top')),       # Where to place labels in the rectangle?
        overlap.labels=0.5,               
        inflate.labels=F)

# Amount of sales in each day.
g5 <- df %>% group_by(InvoiceNo) %>%
  distinct(date) %>%
  group_by(date) %>%
  summarise(purchases = n()) %>%
  ggplot(aes(x = date, y = purchases)) +
  geom_line(color="#69b3a2") +
  geom_point() + 
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09")),
               date_breaks = "2 month", date_labels = "%b %Y") +
  scale_y_continuous(limit=c(0,150)) +
  ggtitle('Vendas por dia')+
  xlab('')+
  ylab('')

# Amount of profit in each day.
g6 <- df %>% mutate(profit = Quantity*UnitPrice) %>% 
  group_by(date) %>%
  summarise(profit_day = sum(profit)) %>%
  ggplot(aes(x = date, y = profit_day)) +
  geom_line(color="#69b3a2") +
  geom_point() +
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09")),
               date_breaks = "2 month", date_labels = "%b %Y") +
  ggtitle('Receita por dia')+
  xlab('')+
  ylab('')

# Quantity of produts sold in each day.
g7 <- df %>% 
  group_by(date) %>%
  summarise(quantity_day = sum(Quantity)) %>%
  ggplot(aes(x = date, y = quantity_day)) +
  geom_line(color="#69b3a2") +
  geom_point() + 
  scale_x_date(limit=c(as.Date("2010-12-01"),as.Date("2011-12-09")),
               date_breaks = "2 month", date_labels = "%b %Y") +
  ggtitle('Produtos vendidos por dia')+
  xlab('')+
  ylab('')

gridExtra::grid.arrange(g5,g6,g7,nrow = 3)

# In the next script, named 3-MBA_Apriori_Algorithm.R, the association rules will 
# be explored. See you there!

