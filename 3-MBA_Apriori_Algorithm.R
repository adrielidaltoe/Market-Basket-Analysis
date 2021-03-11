# In this part, I'm going to apply the apriori algorithm to create the association rules

# In order to perform apriori algorithm we need a data set that has transactions and items. A transaction,
# in this context, is how the items are arranged. A single transaction will have one or multiple items.
# The data can be arranged into an item list, in which each object will be a transaction; or a binary 
# (sparse) matrix, where each row represents a transaction. Both structures can be transformed into a 
# transaction object.


setwd("D:/FCD/Projetos/Market_basket_analysis")

library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)

# Importando os dados limpos
df <- read_csv('retail.csv')
View(df)
dim(df) # 528057     11
glimpse(df)


df <- df %>%
  mutate(InvoiceNo = as.factor(InvoiceNo)) %>%
  mutate(Description = as.factor(Description)) %>%
  mutate(Date = as.Date(Date))

# Looking for duplicated entries in the dataset
df$Inv_Desc <- c(paste(df$InvoiceNo, df$Description, sep = ' '))
length(which(duplicated(df$Inv_Desc))) #  10794 lines were duplicated
length(unique(df$Inv_Desc))
df <- df[!duplicated(df$Inv_Desc), ]
dim(df) # 517263     11

# Calculating the supporting values for the products

# Proportion of each item in total volume of sales (total number of products saled)
df %>% group_by(Description) %>% summarize(n = n()) %>% mutate(support = n / sum(n)) %>%
  arrange(desc(support))

# Proportion that each item appeared in each purchase (aprox. support value)
df %>% group_by(Description) %>% summarize(n = n()) %>% mutate(support = n / 19789) %>%
  arrange(desc(support)) %>% filter(Description == 'BLUE POLKADOT BEAKER')




# Apriori uses these support values

## Creating the item list and obtaining the transaction object
transaction <- split(df$Description,df$InvoiceNo)
class(transaction)

basket <- as(transaction, 'transactions')
summary(basket)

## Creating a csv file. THE RESULT DID NOT CORRESPOND TO THE ORIGINAL DATA

transactionData <- ddply(df,c('InvoiceNo', 'Date'),
                         function(df1){paste(df1$Description, collapse = ',')})

# Removing the columns InvoiceNo and Date because they will not be necessary
transactionData <- transactionData %>% select(V1)
colnames(transactionData) <- c('Items')

# Saving the transaction data
write.csv(transactionData, "transacoes.csv", quote = FALSE, row.names = FALSE)

# Now we are ready to apply the apriori algorithm
?read.transactions
transactions <- read.transactions('transacoes.csv', format = 'basket', sep = ",", 
                                  rm.duplicates = FALSE)
summary(transactions)

## Creating a logical matrix
df1 <- df %>% 
  select(InvoiceNo, Description) %>%
  mutate(value = TRUE) %>% 
  spread(Description, value, fill = FALSE) %>%
  select(-InvoiceNo)
View(head(df1[,c(1:10)]))
transaction.obj <- as.matrix(df1) %>% as('transactions')
summary(transaction.obj)

# Create a visualization from the transaction object
library(RColorBrewer)
itemFrequencyPlot(transaction.obj, topN = 10, type = 'absolute', 
                  col=brewer.pal(8,'Pastel2'), 
                  main = 'Absolute Item Frequency Plot')


### Applying the apriori algorithm
# Finding the association rules
?apriori
rules <- apriori(transaction.obj, parameter = list(supp=0.01,maxlen=10))
summary(rules)
rules <- sort(rules, by='lift', decreasing = TRUE)
inspect(rules[1:10])

rules1 <- apriori(transaction.obj, parameter = list(supp=0.002, maxlen = 3))
rules1 <- sort(rules1, by='support', decreasing = TRUE)
summary(rules1)
inspect(rules1[1:10])


## Association rules for 
whiteHang.rules = apriori(transaction.obj, parameter = list(supp=0.003),
                          appearance = list(lhs="WHITE HANGING HEART T-LIGHT HOLDER",default="rhs"))

whiteHang.rules = apriori(transaction.obj, parameter = list(supp=0.003, conf = 0.8, maxlen = 2),
                          appearance = list(default="lhs", rhs="WHITE HANGING HEART T-LIGHT HOLDER"))

summary(whiteHang.rules)
whiteHang.rules <- sort(whiteHang.rules, by = 'lift', decreasing = TRUE)
inspect(whiteHang.rules)

# Visualizing Association Rules
plot(rules)
plot(rules1)

# Two-key-point
plot(rules, method = 'two-key plot')
plot(rules1, method = 'two-key plot')

# Graph-based visualizations
plot(rules[1:20], method = "graph",  engine = "htmlwidget")
