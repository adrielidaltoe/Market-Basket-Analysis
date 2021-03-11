#########################################################################################


        # Market Basket Analysis - Part 1 - Data Cleansing and Manipulation


#########################################################################################

setwd("D:/DataScienceAcademy/FCD/Projetos/Market_basket_analysis/Market-Basket-Analysis")

# Import packages
library(readxl)
library(dplyr)
library(skimr)


# Import data
df <- read_excel('Online_Retail.xlsx')
View(df)

dim(df) # 541909 lines and 8 columns

# Data Dictionary

# InvoiceNo: Invoice number. If this code starts with letter 'c', it indicates a cancellation.
# StockCode: Item code. Number uniquely assigned to each distinct product.
# Description: Product (item) name.
# Quantity: The quantities of each product (item) per transaction.
# InvoiceDate: The day and time when each transaction was generated.
# UnitPrice: Product price per unit in sterling.
# CustomerID: Number uniquely assigned to each customer.
# Country: Name of the country where each customer resides.


# Summary shows that there are inconsistents in the data. There are negative values 
# in quantity and unit price.
summary(df)

# Looking for missing values. There are missing data in Description and CustomerID attributes.
sapply(df, function(x){sum(is.na(x))})


## Data wrangling/manging

# InvoiceNo. Let's find the cancelled transactions.

df %>% 
  filter(grepl('^c', df$InvoiceNo, ignore.case = TRUE)) %>%
  summarise(n = n())
# in filter use grepl because it returns logical values
# 9288 transactions were cancelled.

# Removing cancelled transactions from the data
df <- df %>% 
  filter(!grepl('^c', df$InvoiceNo, ignore.case = TRUE))

dim(df) # 532621 rows and 8 columns

# There is also invoiceNo with letter A, indicating Adjust of bad debt, which are
# related with the nevative values of UnitPrice that did not make sense in the data.
# Finding the row index of these occurrences 
index <- which(is.na(as.double(df$InvoiceNo))) 
df[index,]

# Remove these transactions
df <- df[-index, ]

summary(df)
# Now, the negative values in UnitPrice were removed.


## Exploring the Quantity attribute. I will look for the cases where quantity was <= 0.
# There is no sale with 0 units of a product.
df %>% filter(Quantity <= 0) %>% summarise(n=n())

View(df %>% filter(Quantity <= 0) %>% select(Description))

# It seems that these transactions were referred to products that were demaged. The data will be removed.
# Invoices of only quantity > 0 will be kept.
df <- df %>% filter(Quantity > 0)

dim(df) # Remaining 531282 rows and 8 columns
summary(df)


## Description attribute

# Let's verify the missing data
sapply(df, function(x){sum(is.na(x))})

View(df %>% filter(is.na(Description)))

# There are two options to treat the missing data of description attribute:
# 1. Remove the rows
# 2. Find the description of the products using the StockCode. 
# Because the data has a considerably large amount of samples and the missing data in 
# Description corresponds to 0.3% of the data, I will remove those rows.

df <- df %>% filter(!is.na(Description))
dim(df) # Remaining 530690 rows and 8 columns


## StockCode attribute

# Looking for stockcodes that do not correspond to numbers.
View(df %>% filter(grepl('^[A-z]', StockCode, ignore.case = TRUE)) %>% 
  group_by(StockCode, Description) %>% summarise(n = n()) %>% arrange(desc(n)))

# Let's remove those items because they are not products
df <- df %>% filter(!grepl('^[A-z]', StockCode, ignore.case = TRUE))
summary(df)

dim(df) # Remaining 528302 rows and 8 columns

# Looking for inconsistencies in Description

# Let's start by UnitPrice == 0. Are these items gifts?
sample <- df[df$UnitPrice ==0, ]
View(sample)

# There are lots of legitimate products in that list, but there are some descriptions in 
# lower case that are clearly not products.
View(sample %>% filter(grepl('[a-z]', Description)) %>% distinct(Description))

notProduct_description <- sample %>% filter(grepl('[a-z]', Description)) %>%
  distinct(Description)

notProduct_description <- notProduct_description$Description[-c(2,7,8)]

df <- df %>% filter(!Description %in% notProduct_description)

dim(df) #Remaining 528149 rows and 8 columns

View(df %>% distinct(Description))

# Descriptions that might not be related with products
strings =  c('?','FOUND','\\ttest','AMAZON','Next Day Carriage',
             'Dotcomgiftshop Gift Voucher £100.00')

# Removing all the entries with the strings above

df <- df %>% filter(!Description %in% strings)
summary(df)
dim(df) # Remaining 528059 rows


# In this project I want to find association rules between the products, then I will use only
# InvoiceNo and Description to the analysis. In this way the missing values in CustomerID do 
# not need to be treated.

# Saving the dataframe
write.csv(df, file = 'retail.csv', row.names = FALSE)

# In script 2-Exploratory_data_analysis.R you find part-2 of this project.
