library(ggplot2)
library(animation)
library(tidyr)
library(RColorBrewer)
library(fmsb)
################### Data 1. Time Spent on the TV Channels #######################
data <-read.csv("D:\\Projects\\VMF_Regression\\data\\timeSpentTV.csv",header=T)
#summary(data)

timeSpent<- data[,c(2:9)]##only select columns needed


# ### set anything above the 95th percentile to the 95th percentile value
# trimData <- function(x){
#   topLimit <<- quantile( x, c(0.95 ))
#   print(topLimit)
#   return(topLimit)
# }
# 
# for(col in 1:ncol(timeSpent)){
#   print(col)
#   trimData(timeSpent[,col])
#   for(row in 1:nrow(timeSpent)){ 
#     if(timeSpent[row,col] > topLimit){timeSpent[row,col]<- topLimit}
#   }
# }

##turn into percentile values
for(col in 1:ncol(timeSpent)){
  timeSpent[,col]<- percentile( timeSpent[,col])
}
summary(timeSpent)
### set the lowest percentile to almost zero to make the results easier to understand
# for(row in 1:nrow(timeSpent)){
#   if(timeSpent$ALBA[row] == 97.00){timeSpent$ALBA[row] = 0.01}
#   if(timeSpent$CBBC[row] == 92.00){timeSpent$CBBC[row] = 0.01}
#   if(timeSpent$CBEEBIES[row] == 85.00){timeSpent$CBEEBIES[row] = 0.01}
#   if(timeSpent$FOUR[row] == 67.00){timeSpent$FOUR[row] = 0.01}
#   if(timeSpent$NEWS[row] == 70.00){timeSpent$NEWS[row] = 0.01}
#   if(timeSpent$PARLIAMENT[row] == 94.00){timeSpent$PARLIAMENT[row] = 0.01}
#   if(timeSpent$TWO[row] == 97.00){timeSpent$TWO[row] = 0.01}
# }
######## Using the elbow plot to find a good number of clusters ####
clusterNums <-2:20
tries <- 100
avg_wssCluster<- integer(length(clusterNums)) #make an empty list to hold results


## For each cluster number, run kmeans 100 times, store the wss and then average
for(v in clusterNums){
  wssCluster_v <- integer(tries) 
  for(i in 1:tries){
    k.temp <- kmeans(timeSpent[,c(1:8)], centers = v) 
    wssCluster_v[i] <- k.temp$tot.withinss 
  }
  avg_wssCluster[v-1] <-mean(wssCluster_v)
}

## Have a look at results
plot(clusterNums,avg_wssCluster, type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")
## Something around 4 or 5 looks the best

k<- kmeans.ani(timeSpent[,c(1:8)], centers = 4)
#k<- kmeans(timeSpent[,c(2:8)], centers = 5)
k$centers 
table(k$cluster)
summary(timeSpent[,c(2:4)])


### visualise results of kmeans
cluster <- c(1:5)
centerDF <- data.frame(cluster, k$center)

centerReshape <- gather(centerDF, channels, values, ALBA:TWO)
head(centerReshape)

heatMapPalette <-colorRampPalette(rev(brewer.pal(10, 'Spectral')),space='Lab')## colour palette

#heatMapPalette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')## colour palette
ggplot(data = centerReshape, aes(x = channels, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 5, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = heatMapPalette(90)) +
  theme_classic()
##################################################################################

################### Data 2. Platform visited per week #######################