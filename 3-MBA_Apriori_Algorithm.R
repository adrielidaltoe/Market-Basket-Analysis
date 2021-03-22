#########################################################################################


        # Market Basket Analysis - Part 3 - Mining Association Rules


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
library(ggplot2)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)

# In this part, the apriori algorithm will be used to create the association rules

# In order to perform apriori algorithm we need a data set that has transactions and items. 
# A single transaction will have one or multiple items. The data can be arranged into an
# item list, in which each object will be a transaction; or a binary (sparse) matrix, 
# where each row represents a transaction and items are distributed in columns.
# Both structures can be transformed into a transaction object.



# Importando os dados limpos
df <- read_csv('retail_clean.csv')
View(df)
dim(df)


# Arules requires the data to be in the form of transactions, where each row represent a transaction
# and each product is a column. Here I show two ways of doing that:

# 1) Create a sparse matrix and then transform it into an arule transactions object
sparce_matrix <- df %>% 
  select(InvoiceNo, Description) %>%
  mutate(value = 1) %>% 
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo)

View(sparce_matrix)

transaction_matrix <- as(as.matrix(sparce_matrix), 'transactions')
class(transaction_matrix)
summary(transaction_matrix)

# 2) Create an item list and then the arule transactions object
transaction <- split(df$Description,df$InvoiceNo)
class(transaction)

basket <- as(transaction, 'transactions')
class(basket)
bask_summary <- summary(basket)

# Both methods result in the same arule transctions object.However, the second method is easier to use.
# Let's investigate the transactions object basket.

# There is 19791 transactions and a total of 3994 different products
bask_summary@Dim

# Checking if basket object (the transaction object) is consistent with the data 
bask_summary@itemSummary
freqItem_support
# Notice that the frequency of top 5 most frequent products in transactions are the same.

# How many items there are in each transaction?
bask_summary@lengths

### Mining the association rules with Apriori
# The number of rules will be controlled by setting values for support, confidence and
# the maximum number of items in a rule.

rules <- apriori(basket, parameter = list(supp=0.01, conf = 0.7))

# 254 rules were found 
summary(rules)
inspect(head(rules, n = 10, by ="lift"))

# Visualize rules in a dataset
rules_dataframe <- DATAFRAME(rules)[, -c(5)]
View(rules_dataframe[c(1:10)*10,] %>%
       arrange(desc(lift)))


# Visualizing Association Rules
plot(rules, main = '254 regras de associação')

# Two-key-point
plot(rules, method = 'two-key plot', main = 'Número de itens por regra de associação')


# Graph-based visualizations
plot(rules, method = "graph",  engine = "htmlwidget")
rulesinspect(rules[1:20])


# Removing subset of rules
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 135
rules_filtered <- rules[-subsetRules] # remove subset rules. 
summary(rules_filtered)
View(DATAFRAME(rules_filtered)[, -c(5)] %>%
       arrange(desc(lift)))

plot(rules_filtered, method = "graph",  engine = "htmlwidget")
