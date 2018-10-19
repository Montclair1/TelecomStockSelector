library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity

setwd("C:/Users/tknee/Desktop/MiningProject") ## set working directory
data <- read.csv("TelcomNorm.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
#drop index column, store as new dataframe
data2 = select(data,-X, -Date)

# get statistical summary of data set
summary(data2)

# create single boxplot
boxplot(data2$CMCSA)
#create boxplots of entire data set
# cex changes font size
boxplot(data2,main="Telecom Stocks, Daily Change in Price, (2013-17)",cex.main=1.5,cex.axis=1.2,geom="point")

#plot for Verizon price histogram
ggplot(data=data2, aes(data2$VZ)) + 
  geom_histogram(breaks=seq(-.04, .04, by = .002), 
                 col="black", 
                 fill="blue", 
                 alpha = .8)+theme(plot.title = element_text(hjust = 0.5))+
   labs(title="Verizon Daily Price Change (logNormal)",x="Daily Change", y= "Frequency")+
  scale_x_continuous(breaks=c(-.04,-.03,-.02,-.01,0.0,.01,.02,.03,.04), labels=c("-.04","-.03","-.02","-.01","0",".01",".02",".03",".04"))+
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150), labels=c("0","25","50","75","100","125","150"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"))+
  theme(plot.title = element_text(size=22,face="bold"))
#q <- ggplot(data2,aes(data2$CMCSA))+geom_histogram(col="red",fill="blue",binwidth=.5)
