library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity
library(dplyr) # for manipulating dataframe
library(RColorBrewer)  # for added colors

setwd("C:/Users/tknee/Desktop/TelecomStockSelector") ## set working directory


library(arules)
library(arulesViz)
#create basket for each quarter
quarters <-read.transactions("VerizonCat.csv",format="basket",sep=",",cols=c(1))
inspect(quarters)

#calculate Support and see frequent items
# this is a different package.  USe below arulesViz!!!
#frequentItems <- eclat(quarters,parameter=list(supp=.15,maxlen=15))
#inspect(frequentItems)
#itemFrequencyPlot(items(frequentItems),topN=10,type="absolute",main="Item Frequency, Support=.15",cex.names=.9,col=brewer.pal(8,'Pastel2'))  # or "relative" type

# Get rules for all of dataset
rules <- apriori(quarters,parameter = list(supp=0.10,conf=0.5,target="rules"))
# High Confidence Rules
rules_conf <- sort(rules, by="confidence",decreasing=TRUE)
inspect(head(rules_conf,10)) 
# High Lift Rules
rules_lift <- sort(rules, by="lift",decreasing=TRUE)
#subrules2 <- head(rules_lift)
inspect(head(rules_lift,10))
#plot(rules_lift, measure=c("support","lift"),shading="confidence",jitter=0)
# plot(subrules2, method = "two-key plot")
rules_lift1=head(rules_lift,10)
plot(rules_lift1, method = "graph") #YES
plot(rules_lift, method = "matrix3D")  # Possible
plot(rules_lift, method = "grouped",gp_labels = gpar(col="dark blue",cex=1,fontface="bold"))  # Use this to figure out rules  
plot(rules_lift, method = "paracoord") 
inspectDT(rules_lift) # YES USE THIS

# SINGLE RULE VISUALIZATIONA
plot(rules_lift, method = "mosaic") # mosaic can only visualize one rule  YES THIS CONFIRMS FINDINGS

# Find High Confidence Rules
inspectDT(rules_conf) # Use this graphic

# Find High Lift Rules
rules_lift <- sort(rules, by="lift", decreasing=TRUE)
inspectDT(rules_lift)  #  Use this

# FIND ASSOCIATION RULES FOR SPECIFIC PRICE OUTCOMES
# Since there are only 47 rows, these have to have very low support!!!!!  (since 6 categories)

#EXTREMELY POSITIVE
price_rulesEPos <- apriori(data=quarters,parameter=list(supp=.05,conf=.3),appearance=list(default="lhs",rhs="Price=Extremely Positive"),control=list(verbose=F))
price_rules_confEPos <- sort(price_rulesEPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_confEPos))   # If doesnt display, lower support or confidence
price_rules_liftEPos <- sort(price_rulesEPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_liftEPos))
#visualize top rule
r1 <- sample(price_rules_confEPos,1)
inspect(r1)
plot(r1, method = "mosaic",data=quarters,control=list(main='Price'))  #Title not working

# VERY POSITIVE
price_rulesVPos <- apriori(data=quarters,parameter=list(supp=.05,conf=.5),appearance=list(default="lhs",rhs="Price=Very Positive"),control=list(verbose=F))
price_rules_confVPos <- sort(price_rulesVPos, by="confidence",decreasing=TRUE)
inspect(head(price_rules_confVPos))
r2 <- sample(price_confVPos,1)
inspect(r2)
plot(r2, method = "mosaic",data=quarters,col="blue",col=grey.colors) # mosaic can only visualize one rule

# POSITIVE   4 RULES FOUND
price_rulesPos <- apriori(data=quarters,parameter=list(supp=.05,conf=.6),appearance=list(default="lhs",rhs="Price=Positive"),control=list(verbose=F))
price_rules_confPos <- sort(price_rulesPos, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_confPos))   # NOt displaying
price_rules_liftPos <- sort(price_rulesPos, by="lift", decreasing=TRUE)
inspect(head(price_rules_liftPos))
#visualize top rule
r3 <- sample(price_rules_confPos,1)
inspect(r3)
plot(r3, method = "mosaic",data=quarters,control=list(main='Price'))  #Title not working

# NEGATIVE  # 1 RULE FOUND NOT GREAT
price_rulesNeg <- apriori(data=quarters,parameter=list(supp=.05,conf=.5),appearance=list(default="lhs",rhs="Price=Negative"),control=list(verbose=F))
price_rules_confNeg <- sort(price_rulesNeg, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_confNeg))   # NOt displaying
price_rules_liftNeg <- sort(price_rulesNeg, by="lift", decreasing=TRUE)
inspect(head(price_rules_liftNeg))
#visualize top rule
r4 <- sample(price_rules_confNeg,1)
inspect(r4)
plot(r4, method = "mosaic",data=quarters,control=list(main='Price'))  #Title not working

# VERY NEGATIVE
price_rulesVNeg <- apriori(data=quarters,parameter=list(supp=.05,conf=.5),appearance=list(default="lhs",rhs="Price=Very Negative"),control=list(verbose=F))
price_rules_confVNeg <- sort(price_rulesVNeg, by="confidence",decreasing=TRUE)  
inspect(head(price_rules_confVNeg))   # NOt displaying
price_rules_liftVNeg <- sort(price_rulesVNeg, by="lift", decreasing=TRUE)
inspect(head(price_rules_liftVNeg))
#visualize top rule
r5 <- sample(price_rules_confVNeg,1)
inspect(r5)
plot(r5, method = "mosaic",data=quarters,control=list(main='Price'))  #Title not working

#EXTREMELY NEGATIVE  # NOTHING STANDS OUT, ALL LOW SUPPORT (1 count each)
price_rulesENeg <- apriori(data=quarters,parameter=list(supp=.01,conf=.3),appearance=list(default="lhs",rhs="Price=Extremely Negative"),control=list(verbose=F))
price_rules_confENeg <- sort(price_rulesENeg, by="confidence",decreasing=TRUE)
inspect(head(price_rules_confENeg))
price_rules_liftENeg <- sort(price_rulesENeg, by="lift", decreasing=TRUE)
inspect(head(price_rules_liftENeg))
#visualize top rule
r6 <- sample(price_rules_confENeg,1)
inspect(r6)
plot(r6, method = "mosaic",data=quarters,control=list(main='Price'))  #Title not working


# Remove redundant rules  NOT HELPING AT ALL ELMINATES ALL RULES
subsetRulesPos <- which(colSums(is.subset(price_rules_liftPos,price_rules_liftPos))>1)
length(subsetRulesPos)
price_rules_liftPos <- price_rules_liftPos[-subsetRules] 
inspect(head(price_rules_liftPos))
