library(ggplot2)
library(animation)
library(tidyr)
library(RColorBrewer)
##### Data 1. Time Spent on the TV Channels ######
data <-read.csv("D:\\Projects\\VMF_Regression\\data\\Time Spent Channels\\timeSpentChannel.csv",header=T)
summary(data)

timeSpent<- data[,c(7:14)]##only select columns needed

### set anything above the 95th percentile to the 95th percentile value
trimData <- function(x){
  topLimit <<- quantile( x, c(0.95 ))
  print(topLimit)
  return(topLimit)
}
for(col in 1:ncol(timeSpent)){
  print(col)
  trimData(timeSpent[,col])
  for(row in 1:nrow(timeSpent)){ 
    if(timeSpent[row,col] > topLimit){timeSpent[row,col]<- topLimit}
  }
}
summary(timeSpent)

######## Using the elbow plot to find a good number of clusters ####
clusterNums <-2:20
tries <- 100
avg_wssCluster<- integer(length(clusterNums)) #make an empty list to hold results


## For each cluster number, run kmeans 100 times, store the wss and then average
for(v in clusterNums){
  wssCluster_v <- integer(tries) 
  for(i in 1:tries){
    k.temp <- kmeans(timeSpent[,c(5:8)], centers = v) 
    wssCluster_v[i] <- k.temp$tot.withinss 
  }
  avg_wssCluster[v-1] <-mean(wssCluster_v)
}

## Have a look at results
plot(clusterNums,avg_wssCluster, type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")
## Something around 4 or 5 looks the best

k<- kmeans.ani(timeSpent[,c(5:8)], centers = 5)
k$centers 
table(k$cluster)
summary(timeSpent[,c(2:4)])


### visualise results of kmeans
cluster <- c(1:5)
centerDF <- data.frame(cluster, k$center)

centerReshape <- gather(centerDF, channels, values, CBBC:PARLIAMENT)
head(centerReshape)

heatMapPalette <-colorRampPalette(rev(brewer.pal(10, 'Spectral')),space='Lab')## colour palette

#heatMapPalette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')## colour palette
ggplot(data = centerReshape, aes(x = channels, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 5, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = heatMapPalette(90)) +
  theme_classic()
