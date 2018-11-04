library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity
library(dplyr) # for manipulating dataframe

setwd("C:/Users/tknee/Desktop/TelecomStockSelector") ## set working directory


library(arules)
#create basket for each quarter
quarters <-read.transactions("VerizonCat.csv",format="basket",sep=",",cols=c(1))
inspect(quarters)

#calculate Support and see frequent items
frequentItems <- eclat(quarters,parameter=list(supp=.001,maxlen=15))
inspect(frequentItems)
itemFrequencyPlot(trans,topN=10,type="absolute",main="Item Frequency")

# Get rules for dataset
rules <- apriori(quarters,parameter = list(supp=0.05,conf=0.1,target="rules"))
# sort rules for high confidence
rules_conf <- sort(rules, by="confidence",decreasing=TRUE)
inspect(head(rules_conf))
# Find High Lift Rules
rules_lift <- sort(rules, by="lift", decreasing=TRUE)
inspect(head(rules_lift))
# find what influenced Specific Price Change:  NO RULES COMING UP! MUST BE GETTING ELIMINATED
# I lowered confidence. NOt giving us anything workable since too few rows
price_rules <- apriori(data=quarters,parameter=list(supp=.05,conf=.1),appearance=list(default="lhs",rhs="Price=Very Positive"),control=list(verbose=F))
price_rules_conf <- sort(price_rules, by="confidence",decreasing=TRUE)
inspect(head(price_rules_conf))
price_rules_lift <- sort(price_rules, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift))

# Remove redundant rules  NOT HELPING AT ALL ELMINATES ALL RULES
subsetRules <- which(colSums(is.subset(price_rules_lift,price_rules_lift))>1)
length(subsetRules)
price_rules_lift <- price_rules_lift[-subsetRules] 
inspect(head(price_rules_lift))
