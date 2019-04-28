library(arules)
library(arulesViz)
library(tidyverse)

data("Groceries")

head(Groceries)

grocery <- as(Groceries, 'data.frame')
head(grocery)

summary(Groceries)

# Density of 0.026 means 2.6 % are non-zero matrix cells. 
# Dimensions: 9835 * 169 
# Whole milk was purchased 2513 times which is 26% of the transactions 
# 2519 transactions contained 1 item , while only 1 transaction had 32 items 
# First quartile and median is 2 and 3 , which means that 25% of the transactions had 2 items and about half contained 3 items 

itemFrequency(Groceries[,1:5])

itemFrequencyPlot(Groceries, support= 0.10)
#8 items have 10% of the support 
itemFrequencyPlot(Groceries, support= 0.05)
# 28 items have atleast 5 % of the support 

#Relative frequency of top 20 items 
itemFrequencyPlot(Groceries, topN = 20)

#Visualizing first 5 transactions 
image(Groceries[1:5])

#Random 100 transactions 
image(sample(Groceries, 100))

#Apriori algorithm 
# finding items that are sold three times a day, therefore for a monthm, support = 90/9835
basket <- apriori(Groceries, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
basket

summary(basket)

#gave a set of 224 rules , rule length distribution gives us how many items are present in how many rules 

# 2 items are present in 111 rules 
# 3 items are present in 113 rules 

inspect(basket[1:10])

#The first five rules are seen here. Also, we can see support for the top 5 most frequent items. We can see the lift 
#column along with support and confidence. The lift of a rule measures how much likely an item or itemset is 
#purchased relative to its typical rate of purchase, given that you know another item or itemsethas been purchased.

# Sorting according to lift 
inspect(sort(basket, by = "lift")[1:5]) 

#People who buy berries have 4 times tendency to buy whipped/sour cream than other customers 

#taking subsets of Arules 
#Sometimes the marketing team requires to promote a specific product, say they want to promote berries, and want 
#to find out how often and with which items the berries are purchased. The subset function enables one to find 
#subsets of transactions, items or rules. The %in% operator is used for exact matching

#Suppose I want to see it for berries

berries <- subset(basket, items %in% "berries")
inspect(berries)

#Yoghurt and whipped/sour cream turned up with which Berries is purchased

#Scatter Plot for 224 rules 
plot(basket)

plot(basket, measure=c("support", "lift"), shading="confidence")


#Shading by order (number of items contained in the rule)

plot(basket, shading="order", control=list(main = "Two-key plot"))

#Interactive Scatter Plot 

plot(basket, measure=c("support", "lift"), shading="confidence", interactive=TRUE)


# group based visualization 
plot(basket, method="grouped")

# graph based visualization
plot(basket, method="graph", control=list(type="items"))
