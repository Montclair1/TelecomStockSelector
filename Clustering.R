library(lubridate) ## for dates
library(dplyr) # for manipulating dataframe
library(ggplot2)  # for graphics
library(scales) # pretty_breaks , etc
library(gtools)  ## for testing validity
library(dplyr) # for manipulating dataframe
library(RColorBrewer)  # for added colors

# New packages
library(animation) # visualizing cluster process
library(tidyverse) # data manipulation
library(cluster)  # clustering algorithms
library(factoextra)  # clustering
library(NbClust)      # more clustering



setwd("C:/Users/tknee/Desktop/TelecomStockSelector") ## set working directory

# stock data
data <- read.csv("TelcomNorm.csv", header = TRUE, stringsAsFactors = FALSE, sep=",") 
data <- select(data,-X, -Date) #drop index and date columns
#------------------------------------------------------------------------------------------------------
# Assess distances visually Takes a while to load
distance <- get_dist(data)
fviz_dist(distance, gradient=list(low="#00AFBB",mid="white",high="#FC4E07"))


# Kmeans Clustering by multiple factors
fit <- kmeans(data[3:12],3,nstart=25) 
fit  # view results
# Try with only telecom stocks
fit2 <- kmeans(data[3:7],3,nstart=25) 
fit2

fit3 <- kmeans(data[3:7],5,nstart=25) 
fit3

fit4 <- kmeans(data[3:7],10,nstart=25) 
fit4

# Create table to measure how well clusters performed
IYZ_table <- table(data$IYZ_Perf,fit$cluster)
IYZ_table
# Create table to measure how well clusters performed (VZ not as good, as expected)
VZ_table <- table(data$VZ_Perf,fit$cluster)
VZ_table

# Display Cluster Plot
p1<- fviz_cluster(fit, data= data[3:12],geom="point",pointsize=.6,main="All factors")
p2<- fviz_cluster(fit2, data= data[3:7],geom="point",pointsize=.6,main="Stocks only")
p3<- fviz_cluster(fit3, data= data[3:7],geom="point",pointsize=.6,main="k=5 Clusters")
p4<- fviz_cluster(fit4, data= data[3:7],geom="point",pointsize=.6,main="k=10 Clusters")

library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2)

# Should we test all ETFs and then measure against performance of VZ?



# visualize multiple dimensions
pca_res <- prcomp(as.matrix(data[3:12]),center=TRUE, scale. = TRUE)  # reduces dimensions with PCA
# Assess how well clustering matches ETF basket labels
plot <- cbind(as.data.frame(pca_res$x[,1:2]),labels=data$IYZ_Perf)  # visualize as 2D
ggplot(plot,aes(x=PC1,y=PC2, color=labels, center=FALSE,scale=FALSE)) +geom_point()+ ggtitle("ETF Basket Labelling")+theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=.5))
# experiment with center=TRUE, scale=TRUE

# Second method, only individual stocks
pca_res2 <- prcomp(as.matrix(data[3:7]),center=TRUE, scale. = TRUE)  # reduces dimensions with PCA
plot2 <- cbind(as.data.frame(pca_res2$x[,1:2]),labels=data$IYZ_Perf)  # visualize as 2D
ggplot(plot2,aes(x=PC1,y=PC2, color=labels)) +geom_point()+ ggtitle("ETF Basket Labelling2")+theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=.5))



# Assess how well clustering matches Verizon labels
plot_VZ <- cbind(as.data.frame(pca_res$x[,1:2]),labels=data$VZ_Perf)  # visualize as 2D
ggplot(plot_VZ,aes(x=PC1,y=PC2, color=labels)) +geom_point()+ ggtitle("Verizon Labelling")+theme(plot.title = element_text(lineheight=2.0, size=32, face="bold",hjust=.5))


# Measure performance of Verizon vs Nasdaq
# Shows positive correlation
# ADD REGRESSION LINE
#data$VZ <- as.factor(data$VZ)
ggplot(data,aes(VZ,QQQ, color=VZ_Perf)) +geom_point() ## choose parameters here
mod1 <- lm(data$VZ ~ data$QQQ)  # not working
abline(mod1, lwd=2)

#----------------------------------------------------------------------
# Kmedoid Clustering by multiple factors
fitMed1 <- pam(data[3:7],3, metric="euclidean") 
fitMed1  # view results

fitMed2 <- pam(data[3:7],3, metric="manhattan")
fitMed2

fitMed3 <- pam(data[3:7],5, metric="manhattan")
fitMed3

fitMed4 <- pam(data[3:7],10,metric="manhattan")
fitMed4

# Create table to measure how well clusters performed
# Test all ETFs find best performer
IYZ_table_KMed <- table(data$IYZ_Perf,fitMed1$cluster)
IYZ_table_KMed
# CCompare with Manhattan Distance
IYZ_table_KMed2 <- table(data$IYZ_Perf,fitMed2$cluster)
IYZ_table_KMed2

# Display Cluster Plot
p1M<- fviz_cluster(fitMed1, data= data[3:7],geom="point",pointsize=.6,main="K-Medoid: Euclidean")
p2M<- fviz_cluster(fitMed2, data= data[3:7],geom="point",pointsize=.6,main="K-Medoid: Manhattan")
p3M<- fviz_cluster(fitMed3, data= data[3:7],geom="point",pointsize=.6,main="k=5 Clusters:KMedoid")
p4M<- fviz_cluster(fitMed4, data= data[3:7],geom="point",pointsize=.6,main="k=10 Clusters:KMedoid")

grid.arrange(p1M,p2M,p3M,p4M, nrow=2)

#-----------------------------------------------------------------------------------------------
# CLARA Clustering by multiple factors
fitC1 <- clara(data[3:7],3, metric="euclidean", samples=5) 
fitC1  # view results

fitC2 <- clara(data[3:7],3, metric="manhattan", samples=5)
fitC2

fitC3 <- clara(data[3:7],5, metric="manhattan",samples=5)
fitC3

fitC4 <- clara(data[3:7],10,metric="manhattan",samples=5)
fitC4

# Create table to measure how well clusters performed
# Test all ETFs find best performer
IYZ_table_C <- table(data$IYZ_Perf,fitC1$cluster)
IYZ_table_C
# CCompare with Manhattan Distance
IYZ_table_C2 <- table(data$IYZ_Perf,fitC2$cluster)
IYZ_table_C2

# Display Cluster Plot
p1C<- fviz_cluster(fitC1, data= data[3:7],geom="point",pointsize=.6,main="Clara sampling: Euclidean")
p2C<- fviz_cluster(fitC2, data= data[3:7],geom="point",pointsize=.6,main="Clara Sampling: Manhattan")
p3C<- fviz_cluster(fitC3, data= data[3:7],geom="point",pointsize=.6,main="k=5 Clara Sampling")
p4C<- fviz_cluster(fitC4, data= data[3:7],geom="point",pointsize=.6,main="k=10 Clara Sampling")

grid.arrange(p1C,p2C,p3C,p4C, nrow=2)



#-----------------------------------------------------------------------------------------
# Another package
#wssplot(data2) Not working
nc <- NbClust(data[3:12],min.nc=2,max.nc=15,method="kmeans")
fit.km <- kmeans(data[3:12],3,nstart=25)
fit.km$size
fit.km$centers
ct.km <- table(data$IYZ_Perf,fit.km$cluster)
ct.km

distance <- get_dist(data2)
fviz_dist(distance, gradient=list(low="#00AFBB",mid="white",high="#FC4E07"))

# no output
distance2 <- get_dist(data)
fviz_dist(distance2, gradient=list(low="#00AFBB",mid="white",high="#FC4E07"))
#------------------------------------------------------------------------------------
#Verizon Business Metrics
data2 <- read.csv("VerizonNumeric.csv", header = TRUE, stringsAsFactors = FALSE, sep=",") 
data2 <- select(data2,-X,-StockPrice,-BookValueperShare,-LN.Price.Change,-Date) #drop index and date columns
data3 <- select(data2,-LN.Price.Change, -Date) #drop index and date columns


# Data exploration
# test with any two variables
ggplot(data2,aes(Log.Normal.StockPrice,DebtToEquity.Ratio, color=LN.Price.Change)) +geom_point()
pairs(data3,pch=19,lower.panel = NULL)


set.seed(2345)
# demonstrates for 2 variables only
kmeans.ani(data2[,6:10],centers=3) # nstart reports on best
# multiple factors
fit <- kmeans(data2[,6:19],4,nstart=25)
fit
# visualize multiple dimensions
pca_res <- prcomp(as.matrix(data2[,6:19]),center=TRUE, scale. = TRUE)  # reduces dimensions with PCA
plot <- cbind(as.data.frame(pca_res$x[,1:2]),labels=data2$LN.Price.Change)  # visualize as 2D
ggplot(plot,aes(x=PC1,y=PC2, color=labels)) +geom_point()
