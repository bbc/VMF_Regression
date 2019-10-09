library(tidyverse)
library(DBI)
library(dbplyr)
library("RSQLite")
library(knitr)
library(gridExtra)
library(grid)
library(ggplot2)
library(reshape2)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(data.table)
library(ggpmisc)

## read in labels recorded by compass and wether they relate to bbc or not
platform <- read.csv("D:\\Projects\\VMF_Regression\\PLATFORMS.csv", header = TRUE)

#### bring in data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
vfm1 <- collect(tbl(con, 'panellstsVMF'))
temp <- tbl(con, 'audienceData12Weeks_incWeb')
audience<- collect(filter(temp, WEEK <= 8,
                          WEEK >= 5))
audienceWeight <- collect(filter(tbl(con, 'panelistsAll12WeeksWEIGHT'), 
                                 WEEK <= 8,
                                 WEEK >= 5))
dbDisconnect(con)

#### average the weight attributed to individuals
weightValue<- audienceWeight %>% 
  filter(startsWith(audienceWeight$INDIVIDUAL_ID, 'I') |startsWith(audienceWeight$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(ID,WEIGHT) %>% 
  group_by(ID) %>% 
  summarise(avgWeight = mean(WEIGHT)) %>%
  distinct()

## remove leading I or H on ID
audienceBBC<- audience %>% 
  filter(startsWith(audience$INDIVIDUAL_ID, 'I') |startsWith(audience$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>%
  distinct()

numWeeksAudience <- audienceBBC %>% 
  group_by(ID)%>% 
  summarise(numWeeksTotal = length(unique(as.character(WEEK))))

  vfmAll <- vfm1 %>%mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID, -SKY_VMF, -VIRGIN_VMF) %>%
  distinct()

vfmFinal<- inner_join(vfmAll, weightValue, by = "ID") %>%
  mutate(AGEGROUP = factor(cut(AGERAW, breaks = c(16,24,34,44,54,64,100), 
                               labels = c("16-24", "25-34","35-44","45-54", "55-64", "65+")))
         ) %>%
  select(-AGERAW)


## Split viewing into platform
platformData<- inner_join(audienceBBC, platform, by = "STREAM_LABEL")
moreThan3Mins<- platformData %>% 
  group_by(ID, WEEK, PLATFORM, STREAM_LABEL) %>%
  summarise(totalDuration = sum(as.numeric(hms(DURATION)))) %>%
  filter(totalDuration > 180)

temp<-moreThan3Mins %>% 
  select(ID, WEEK, PLATFORM)%>%
  distinct() %>%
  group_by(ID, WEEK)%>%
  mutate(numPlatform = length(unique(PLATFORM)))

view(temp%>% filter(numPlatform >5 ))


numPlatform<- moreThan3Mins %>% 
  select(ID, WEEK, PLATFORM)%>%
  distinct() %>%
  group_by(ID, WEEK)%>%
  mutate(numPlatform = length(unique(PLATFORM)))%>%
  select(-PLATFORM) %>%
  distinct() %>%
  group_by(ID)%>%
  mutate(avgNumPlatform = mean(numPlatform)) %>%
  select(ID, avgNumPlatform) %>%
  distinct()

numPlatformVFM<- inner_join(numPlatform, vfmFinal, by = "ID")

write.csv(numPlatformVFM, "D:\\Projects\\VMF_Regression\\data\\numPlatformVFM.csv",row.names = FALSE)


##### spread platforms ####
weeklyVisitsPlatforms <- moreThan3Mins %>% 
  select(ID, WEEK, PLATFORM)%>% ## name platforms visited that week
  distinct() %>%
  group_by(ID, WEEK, PLATFORM)%>%
  mutate(platformInstance = 1) %>%
  group_by(ID,PLATFORM) %>%
  summarise(numWeeksPlatform = length(platformInstance))%>% ##how many weeks did they visit that paltform at least once
  inner_join(numWeeksAudience%>%filter(numWeeksTotal == 4), by = "ID") %>%
  group_by(ID, PLATFORM)%>%
  summarise(platformPerWeek = numWeeksPlatform/numWeeksTotal) %>% ## give the average visit per week
  spread(PLATFORM, platformPerWeek)%>%
  mutate_all(~replace(., is.na(.), 0))
  


write.csv(weeklyVisitsPlatforms, "D:\\Projects\\VMF_Regression\\data\\weeklyVisitsPlatforms.csv",row.names = FALSE)


ggplot(data= numPlatformVFM, aes(x = avgNumPlatform, y = BBC_VMF)) +
  geom_point()

library(hexbin)
h <- hexbin(numPlatformVFM$avgNumPlatform, numPlatformVFM$BBC_VMF, xbins = 10)
plot(h, colramp= function(n){heat.ob(n,beg=250,end=40)}) 

## there is some relationship, more platforms gives higher vfm but its weak

#### Give a training set of data and a testing set
trainingRows<- sample(1:nrow(numPlatformVFM), 0.7*nrow(numPlatformVFM))
trainingData<- numPlatformVFM[trainingRows,]
testData<- numPlatformVFM[-trainingRows,]

lmModel<- lm(BBC_VMF ~ 
             + avgNumPlatform,
             data = trainingData,
             weights = avgWeight
)
summary(lmModel)
## use model to predict on training set and then view correlation
vfmPrediction<- predict(lmModel, testData)
vfmPredictActual<- data.frame(cbind(actuals=testData$BBC_VMF, predicteds = vfmPrediction)) ## actual VFM and that predicted in one df
correlation_accuracy <- cor(vfmPredictActual)  ## correlation
correlation_accuracy 


#ggplot(data = vfmPredictActual, mapping = aes(y = actuals, x = predicteds))+
#  geom_point()


summary(numPlatformVFM$BBC_VMF)
h1 <- hexbin(vfmPredictActual$actuals, vfmPredictActual$predicteds, xbins = 10)
plot(h1, colramp= function(n){heat.ob(n,beg=250,end=40)}) 


############## Does any one paltform promote VFM ############

whichPlatforms<- moreThan3Mins %>%
  select(ID, PLATFORM, WEEK)%>%
  group_by(ID,PLATFORM)%>%
  mutate(weeksOnPlatform = length(unique(WEEK))) %>%
  group_by(ID, PLATFORM) %>%
  select(-WEEK)%>%
  distinct() 

onEachPlatform<- inner_join(whichPlatforms, numWeeksAudience, by = "ID") 
onEachPlatform$PLATFORM<- as.character(onEachPlatform$PLATFORM)

tv<- onEachPlatform %>%
  filter(startsWith(PLATFORM, 'BBC')| startsWith(PLATFORM, 'NEWS_TV')| startsWith(PLATFORM, 'ALBA'))%>%
  group_by(ID, PLATFORM) %>%
  summarise(value = round(weeksOnPlatform/numWeeksTotal,2)) %>%
  spread(PLATFORM, value) %>%
  mutate_all(~replace(., is.na(.), 0))



platformVFM<- inner_join(tv, vfmFinal, by = "ID")

#write.csv(platformVFM,"D:\\Projects\\VMF_Regression\\data\\platformVFM.csv", row.names = FALSE )


trainingRows<- sample(1:nrow(platformVFM), 0.7*nrow(platformVFM))
trainingData<- platformVFM[trainingRows,]
testData<- platformVFM[-trainingRows,]




lm <- lm(BBC_VMF ~ 
           BBC1+
         BBC2+
         BBC4+
         NEWS_TV+
         BBCPARL+
         ALBA, 
         data = trainingData,
         weights = avgWeight
)
summary(lm)

vfmPrediction<- predict(lmModel, testData)
vfmPredictActual<- data.frame(cbind(actuals=testData$BBC_VMF, predicteds = vfmPrediction)) ## actual VFM and that predicted in one df
correlation_accuracy <- cor(vfmPredictActual)  ## correlation
correlation_accuracy 

h1 <- hexbin(vfmPredictActual$actuals, vfmPredictActual$predicteds, xbins = 10)
plot(h1, colramp= function(n){heat.ob(n,beg=250,end=40)}) 
