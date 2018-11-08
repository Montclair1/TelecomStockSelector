library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity
library(dplyr) # for manipulating dataframe

setwd("C:/Users/tknee/Desktop/TelecomStockSelector") ## set working directory


library(arules)
library(arulesViz)
#create basket for each quarter
quarters <-read.transactions("VerizonCat.csv",format="basket",sep=",",cols=c(1))
inspect(quarters)

#calculate Support and see frequent items
frequentItems <- eclat(quarters,parameter=list(supp=.20,maxlen=15))
inspect(frequentItems)
itemFrequencyPlot(items(frequentItems),topN=10,type="relative",main="Item Frequency")  # or "relative" type

# Get rules for dataset
rules <- apriori(quarters,parameter = list(supp=0.10,conf=0.3,target="rules"))
# sort rules for high confidence
rules_conf <- sort(rules, by="confidence",decreasing=TRUE)
inspect(rules_conf)
inspect(head(rules_conf,10)) # add ,10 at end to get more rules
plot(rules_conf)
subrules2 <- head(rules_conf, n = 10, by = "confidence")
plot(subrules2, method = "graph")
plot(subrules2, method = "matrix3D")
plot(subrules2, method = "mosaic") # mosaic can only visualize one rule
plot(subrules2, method = "grouped")  # Use this to figure out rules


# Find High Lift Rules
rules_lift <- sort(rules, by="lift", decreasing=TRUE)
inspect(head(rules_lift))
# find what influenced Specific Price Change:  NO RULES COMING UP! MUST BE GETTING ELIMINATED
# I lowered confidence. NOt giving us anything workable since too few rows
price_rulesEPos <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Extremely Positive"),control=list(verbose=F))
price_rules_conf_EPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_conf_EPos))   # NOt displaying
price_rules_lift_EPos <- sort(price_rulesEPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift_EPos))

price_rulesVPos <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Very Positive"),control=list(verbose=F))
price_rules_confVPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)
inspect(head(price_rules_confVPos))
price_rules_lift_VPos <- sort(price_rulesVPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift_VPos))

# NOT DONE
price_rulesPos <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Positive"),control=list(verbose=F))
price_rules_conf_EPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_conf_EPos))   # NOt displaying
price_rules_lift_EPos <- sort(price_rulesEPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift_EPos))

# NOTE DONE
price_rulesNeg <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Negative"),control=list(verbose=F))
price_rules_conf_EPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_conf_EPos))   # NOt displaying
price_rules_lift_EPos <- sort(price_rulesEPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift_EPos))

# NOT DONE
price_rulesVNeg <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Very Negative"),control=list(verbose=F))
price_rules_conf_EPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_conf_EPos))   # NOt displaying
price_rules_lift_EPos <- sort(price_rulesEPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift_EPos))

#NOT DONE
price_rulesENeg <- apriori(data=quarters,parameter=list(supp=.10,conf=.3),appearance=list(default="lhs",rhs="Price=Extremely Negative"),control=list(verbose=F))
price_rules_conf <- sort(price_rules, by="confidence",decreasing=TRUE)
inspect(head(price_rules_conf))
price_rules_lift <- sort(price_rules, by="lift", decreasing=TRUE)
inspect(head(price_rules_lift))

# Remove redundant rules  NOT HELPING AT ALL ELMINATES ALL RULES
subsetRules <- which(colSums(is.subset(price_rules_lift,price_rules_lift))>1)
length(subsetRules)
price_rules_lift <- price_rules_lift[-subsetRules] 
inspect(head(price_rules_lift))
