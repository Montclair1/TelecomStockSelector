library(ggplot2) #for graphics
#set file directory
setwd("/Users/mymac/Desktop/DataMiningProject/TelecomStockSelector")

#read the data from a csv file and store it to a data frame
read_data <- read.csv("VerizonPreProcessed.csv", header=TRUE, sep = ",")

#create a barplot for quarterly revenue change in %
#each bar represents the revenue change from one quarter to another
barplot(read_data$Quarterly.Revenue...Change, ylim=c(-10,10), col = "gray", 
      main="Quarterly Revenue % Change", ylab = "Percentages",xlab="Quarters", border="purple")


# Create a barplot for quarterly Net Income Change in %
barplot(read_data$Net.Income...Change, ylim=c(-600,600), col = "gray", 
       main="Net Income % Change", ylab = "Percentages",xlab="Quarters", border="purple"
)      

