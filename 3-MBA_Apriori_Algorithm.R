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
library(gridExtra)

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
# The number of rules will be controlled by setting values for support and confidence.

rules <- apriori(basket, parameter = list(supp=0.01, conf = 0.7))

# 254 rules were found 
summary(rules)
inspect(head(rules, n = 10, by ="lift"))

# Visualize rules in the dataset
rules_dataframe <- DATAFRAME(rules)[, -c(5)]
lhs <- sapply(rules_dataframe$LHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
rhs <- sapply(rules_dataframe$RHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
rules_dataframe['n_items'] <- lhs + rhs

p1 <- ggplot(rules_dataframe, aes(x = support, y = confidence, color = lift)) +
  geom_point(size=2) +
  labs(x = 'suporte',y='confiança')

p2 <- rules_dataframe %>%
  mutate(n_itens= as.factor(n_items)) %>%
  ggplot(aes(x = support, y = confidence, color = n_itens)) +
  geom_point(size=2) + scale_color_brewer(type = 'div',palette = 'Dark2') +
  labs(x='suporte',y='confiança')

grid.arrange(p1, p2, nrow = 1, 
             top = textGrob("Regras de Associação - Base de Dados",gp=gpar(fontsize=15)))

View(rules_dataframe %>%
       arrange(desc(lift)))

# Graph-based plot
plot(rules, method = "graph",  engine = "htmlwidget")
rulesinspect(rules[1:20])


# Removing subset of rules
subsetRules <- which(colSums(is.subset(rules, rules)) > 2) # get subset rules in vector
length(subsetRules)  #> 75
rules_filtered <- rules[-subsetRules] # remove subset rules. 
summary(rules_filtered)
View(DATAFRAME(rules_filtered)[, -c(5)] %>%
       arrange(desc(lift)))

plot(rules_filtered, method = "graph",  engine = "htmlwidget")



## Association rules for German customers
germany <- df %>% filter(Country == 'Germany')

basket_germany <- as(split(germany$Description,germany$InvoiceNo), 'transactions')
summary(basket_germany)

rules_germany <- apriori(basket_germany, parameter = list(supp=0.03, conf = 0.7))
summary(rules_germany)
inspect(rules_germany)

filter_germ_rules <- DATAFRAME(rules_germany) %>% select(- c(coverage)) %>% arrange(desc(count))
lhs <- sapply(filter_germ_rules$LHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
rhs <- sapply(filter_germ_rules$RHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
filter_germ_rules['n_items'] <- lhs + rhs
View(filter_germ_rules[c(1,3,5,7,9,11,14,16,19,21,23,25,26), ])

p3 <- ggplot(filter_germ_rules, aes(x = support, y = confidence, color = lift)) +
  geom_point(size=2) +
  labs(x = 'suporte',y='confiança')

p4 <- filter_germ_rules %>%
  mutate(n_itens= as.factor(n_items)) %>%
  ggplot(aes(x = support, y = confidence, color = n_itens)) +
  geom_point(size=2) + scale_color_brewer(type = 'div',palette = 'Dark2') +
  labs(x='suporte',y='confiança')

grid.arrange(p3, p4, nrow = 1, 
             top = textGrob("Regras de Associação - Alemanha",gp=gpar(fontsize=15)))


# Graph plot
plot(rules_germany, method = "graph", engine = "htmlwidget")


## Association rules for French customers
france <- df %>% filter(Country == 'France')

basket_france <- as(split(france$Description,france$InvoiceNo), 'transactions')
summary(basket_france)

rules_france <- apriori(basket_france, parameter = list(supp=0.05, conf = 0.7))
summary(rules_france)
inspect(head(rules_france, n = 10, by ="lift"))

filter_france_rules <- DATAFRAME(rules_france) %>% select(- c(coverage))
lhs <- sapply(filter_france_rules$LHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
rhs <- sapply(filter_france_rules$RHS, function(x){length(strsplit(as.character(x), ',')[[1]])})
filter_france_rules['n_items'] <- lhs + rhs
View(filter_france_rules[c(1,3,5,7,9,11,14,16,19,21,23,25,26), ])


# Plot rules x lift and rules x number of items
p5 <- ggplot(filter_france_rules, aes(x = support, y = confidence, color = lift)) +
  geom_point(size=2) +
  labs(x = 'suporte',y='confiança')

p6 <- filter_france_rules %>%
  mutate(n_itens= as.factor(n_items)) %>%
  ggplot(aes(x = support, y = confidence, color = n_itens)) +
  geom_point(size=2) + scale_color_brewer(type = 'div',palette = 'Dark2') +
  labs(x='suporte',y='confiança')

grid.arrange(p5, p6, nrow = 1, 
             top = textGrob("Regras de Associação - França",gp=gpar(fontsize=15)))

# Graph plot
plot(rules_germany, method = "graph", engine = "htmlwidget")



