library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity

setwd("C:/Users/tknee/Desktop/MiningProject") ## set working directory
data <- read.csv("TelcomNorm.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
datasub <- slice(data,251:502)
datasub$Date <- as.Date(datasub$Date,format = "%m/%d/%Y")
dataVZ <- read.csv("Telcom.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
dataVZsub <- slice(dataVZ,253:504)
dataVZsub$Date <- as.Date(dataVZsub$Date,format = "%m/%d/%Y")
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

# stock chart ;  lognorm data
ggplot(datasub,aes(Date,VZ,group=1)) + 
  geom_line(color="blue",size=1.1) + ggtitle("Daily price Change: Verizon")+
  labs(x="Date", y= "Daily Price Change (lognorm)")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_y_continuous(breaks = pretty(datasub$VZ, n = 6))+
  theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=.5))+
  theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=20,face="bold"), axis.ticks=element_blank())

# line chart, Verizon price
ggplot(dataVZsub,aes(Date,VZ,group=1.1)) + 
  geom_line(color="blue",size=1.2)+ ggtitle("Closing Price: Verizon")+
  labs(x="Date", y= "Daily Price Change")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_y_continuous(breaks = pretty(dataVZsub$VZ, n = 6))+
  theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=0.5))+
  theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=20,face="bold"))
  