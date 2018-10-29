library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity

setwd("C:/Users/tknee/Desktop/MiningProject") ## set working directory

# transformed data
data <- read.csv("TelcomNorm.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
datasub <- slice(data,251:502)  # select specific rows for good looking chart
datasub$Date <- as.Date(datasub$Date,format = "%m/%d/%Y")  # format date for use in chart

# original data
dataVZ <- read.csv("Telcom.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
dataVZsub <- slice(dataVZ,253:504)
dataVZsub$Date <- as.Date(dataVZsub$Date,format = "%m/%d/%Y")

data2 = select(data,-X, -Date) #drop index column, store as new dataframe for boxplots

# get statistical summary of data set
summary(data2)

# create single boxplot
boxplot(data2$CMCSA)

#create boxplots of entire data set
# cex changes font size
boxplot(data2,main="Telecom Stocks, Daily Change in Price, (2013-17)",cex.main=1.5,cex.axis=1.2,geom="point")

#plot for Verizon price histogram, all years
ggplot(data=data2, aes(data2$VZ)) + 
  geom_histogram(breaks=seq(-.04, .04, by = .002), 
                 col="black", 
                 fill="blue", 
                 alpha = .8)+theme(plot.title = element_text(hjust = 0.5))+
   labs(title="Verizon Daily Price Change (logNormal)",x="Daily Change", y= "Frequency")+
  scale_x_continuous(breaks=c(-.04,-.03,-.02,-.01,0.0,.01,.02,.03,.04), labels=c("-.04","-.03","-.02","-.01","0",".01",".02",".03",".04"))+
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150), labels=c("0","25","50","75","100","125","150"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"),plot.title = element_text(size=22,face="bold"))

# stock chart ;  lognorm data, 2014
ggplot(datasub,aes(Date,VZ,group=1)) + 
  geom_line(color="blue",size=1.1) + ggtitle("Daily price Change: Verizon")+
  labs(x="Date", y= "Daily Price Change (lognorm)")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_y_continuous(breaks = pretty(datasub$VZ, n = 6))+
  theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=.5))+
  theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=20,face="bold"), axis.ticks=element_blank())

# line chart, Verizon price, 2014
ggplot(dataVZsub,aes(Date,VZ,group=1.1)) + 
  geom_line(color="blue",size=1.2)+ ggtitle("Daily Closing Price: Verizon")+
  labs(x="Date", y= "Daily Price Change")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_y_continuous(breaks = pretty(dataVZsub$VZ, n = 6))+
  theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=0.5))+
  theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=20,face="bold"))

pairs(dataVZsub,pch=19)
pairs(data2,pch=19,lower.panel = NULL)
#--------------------------------------------------------------------------

# line chart, Comcast price, 2014
ggplot(dataVZsub,aes(Date, CMCSA,group=1.1)) + 
  geom_line(color="red",size=1.2)+ ggtitle("Daily Closing Price: Comcast")+
  labs(x="Date", y= "Daily Price Change")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_y_continuous(breaks = pretty(dataVZsub$CMCSA, n = 6))+
  theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=0.5))+
  theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=20,face="bold"))

# qq plot
ggplot(dataVZsub, aes(sample = VZ)) +stat_qq() +stat_qq_line()

# Melt the correlation matrix  NOT WORKING
# library(reshape2)
# melted_data <- melt(data2, na.rm = TRUE)
# # Heatmap
# ggplot(data = melted_data, aes(variable, value, fill = value))+
#   geom_tile(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Pearson\nCorrelation") +
#   theme_minimal()+ 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 12, hjust = 1))+
#   coord_fixed()
library(ggcorrplot)
corr <- round(cor(data2), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)+ ggtitle("Ordered Correlation Matrix: Telecom Stocks")+theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=0.5))
#-------------------------------------------------------------------------------------------------------
#Categorical Charts
# transformed data
data3 <- read.csv("VerizonPreProcessed.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")  ## import csv
datasub$Date <- as.Date(datasub$Date,format = "%m/%d/%Y")  # format date for use in chart

#plot for Verizon price histogram, all years
ggplot(data=data3, aes(data3$Log.Normal.StockPrice)) + 
  geom_histogram(breaks=seq(-.255, .25, by = .05), 
                 col="black", 
                 fill="green", 
                 alpha = .8)+theme(plot.title = element_text(hjust = 0.5))+labs(title="Verizon Quarterly Price Change (logNormal)",x="Quarterly Change", y= "Frequency")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"),plot.title = element_text(size=22,face="bold"))


data3$LN.Price.Change <- factor(data3$LN.Price.Change, c("Extremely Negative","Very Negative","Negative","Positive","Very Positive","Extremely Positive"))
data3$LN.Price.Change # notice the changed order of factor levels
colnames(data3$data3$Quart.Rev.Pct.Change)<-colnames(data3$Quarterly.Revenue...Change)

g <- ggplot(data=data3, aes(LN.Price.Change))
# Categorical Bar Chart
g + geom_bar(fill = "#426EF4")+theme(plot.title = element_text(hjust = 0.5))+labs(title="Verizon Quarterly Price Change",x="Quarterly Change", y= "Frequency")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"),plot.title = element_text(size=22,face="bold"))

# Quarterly Change Histogram
ggplot(data=data3, aes(data3$Quarterly.Revenue.Pct.Change)) + 
  geom_histogram(breaks=seq(-8, 8, by = 2), 
                 col="black", 
                 fill="green", 
                 alpha = .8)+theme(plot.title = element_text(hjust = 0.5))+labs(title="Verizon Quarterly Revenue Change",x="Quarterly Percent Change", y= "Frequency")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"),plot.title = element_text(size=22,face="bold"))+
  scale_x_continuous(breaks=c(-8,-6,-4,-2,0,2,4,6,8), labels=c("-8","-6","-4","-2","0","2","4","6","8"))+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), labels=c("0","2","4","6","8","10","12","14"))

# HISTOGRAM ROI
ggplot(data=data3, aes(data3$ROI)) + 
  geom_histogram( col="black",fill="blue", alpha = .8, bins =10)+
  theme(plot.title = element_text(hjust = 0.5))+labs(title="Verizon Quarterly ROI",x="Quarterly Percent Change", y= "Frequency")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold"),plot.title = element_text(size=22,face="bold"))+
  scale_x_continuous(breaks=c(-.08,-.06,-.04,-.02,0,.02,.04,.06,.08), labels=c("-.08","-.06","-.04","-.02","0",".02",".04",".06",".08"))+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), labels=c("0","2","4","6","8","10","12","14"))

data3$Quarterly.Revenue.%.Change

