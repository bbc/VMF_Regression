#https://datasharkie.com/how-to-normalize-data-in-r/

mydata<-cars
View(mydata)
summary(mydata)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
## the difference between value x and the minimum divided by the full data range.

mydata$speed_norm<-normalize(mydata$speed)
mydata$dist_norm<-normalize(mydata$dist)

ggplot(data = mydata, mapping = aes(x = speed)) +
  geom_histogram(binwidth = max(mydata$speed)/6, colour="black", fill="#00AFBB") +
  theme_minimal()

ggplot(data = mydata, mapping = aes(x = speed_norm)) +
  geom_histogram(binwidth = max(mydata$speed_norm)/6, colour="black", fill="#00AFBB") +
  theme_minimal()

ggplot(data = mydata, mapping = aes(x = speed, y = dist))+
  geom_point() +
  theme_minimal()
ggplot(data = mydata, mapping = aes(x = speed_norm, y = dist_norm))+
  geom_point() +
  theme_minimal()


normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
x<-rnorm(10,14,2)
y<-rnorm(10,7,3)
z<-rnorm(10,18,5)
df<-data.frame(x,y,z)

df[2:3] <- apply(df[2:3], 2, normFunc)
######################


m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))), 
            nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
                                             items=paste('i', 1:10, sep='')))
r <- as(m, "realRatingMatrix")
r_n1 <- normalize(r) 
r_n2 <- normalize(r, method="Z-score")

r
r_n1
r_n2
########################

dat <- data.frame(x = rnorm(10, 30, .2), y = runif(10, 3, 5))
scaled.dat <- scale(dat)

# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)
