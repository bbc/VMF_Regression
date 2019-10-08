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
pillars <- read.csv("D:\\Projects\\VMF_Regression\\Pillars.csv", header = TRUE)

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

vfmAll <- vfm1 %>%mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID, -SKY_VMF, -VIRGIN_VMF) %>%
  distinct()

vfmFinal<- inner_join(vfmAll, weightValue, by = "ID") %>%
  mutate(AGEGROUP = factor(cut(AGERAW, breaks = c(16,24,34,44,54,64,100), 
                               labels = c("16-24", "25-34","35-44","45-54", "55-64", "65+")))
         ) %>%
  select(-AGERAW)


## Split viewing into pillars
pillarsData<- inner_join(audienceBBC, pillars, by = "STREAM_LABEL")

moreThan3Mins<- pillarsData %>% 
  group_by(ID, WEEK, PILLAR, STREAM_LABEL) %>%
  summarise(totalDuration = sum(as.numeric(hms(DURATION)))) %>%
  filter(totalDuration > 180)

numPillars<- moreThan3Mins %>% 
  select(ID, WEEK, PILLAR)%>%
  distinct() %>%
  group_by(ID, WEEK)%>%
  mutate(numPillars = length(unique(PILLAR)))%>%
  select(-PILLAR) %>%
  distinct() %>%
  group_by(ID)%>%
  mutate(avgNumPillars = mean(numPillars)) %>%
  select(ID, avgNumPillars) %>%
  distinct()

numPillarsVFM<- inner_join(numPillars, vfmFinal, by = "ID")

ggplot(data= numPillarsVFM, aes(x = avgNumPillars, y = BBC_VMF)) +
  geom_point()

library(hexbin)
h <- hexbin(numPillarsVFM$avgNumPillars, numPillarsVFM$BBC_VMF, xbins = 10)
plot(h, colramp= function(n){heat.ob(n,beg=250,end=40)}) 

## there is some relationship, more platforms gives higher vfm but its weak

#### Give a training set of data and a testing set
trainingRows<- sample(1:nrow(numPillarsVFM), 0.7*nrow(numPillarsVFM))
trainingData<- numPillarsVFM[trainingRows,]
testData<- numPillarsVFM[-trainingRows,]

lmModel<- lm(BBC_VMF ~ 
             + avgNumPillars,
             data = trainingData,
             weights = avgWeight
)
summary(lmModel)
## use model to predict on training set and then view correlation
vfmPrediction<- predict(lmModel, testData)
vfmPredictActual<- data.frame(cbind(actuals=testData$BBC_VMF, predicteds = vfmPrediction)) ## actual VFM and that predicted in one df
correlation_accuracy <- cor(vfmPredictActual)  ## correlation
correlation_accuracy 


ggplot(data = vfmPredictActual, mapping = aes(y = actuals, x = predicteds))+
  geom_point()


summary(numPillarsVFM$BBC_VMF)
h1 <- hexbin(vfmPredictActual$actuals, vfmPredictActual$predicteds, xbins = 10)
plot(h1, colramp= function(n){heat.ob(n,beg=250,end=40)}) 
