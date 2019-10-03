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
streamLabels <- read.csv("D:\\Projects\\VMF_Regression\\ALL_STREAM_LABELS.csv", header = TRUE)

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

## only select people in the metadata
audienceBBC <- inner_join(vfmAll %>%select(ID),audienceBBC,  by = 'ID' )

## label each event with the data type e.g BBC_RADIO
audienceBBC_labelled<- inner_join(audienceBBC, streamLabels, by = c('STREAM_LABEL' = 'STREAM_LABEL', 'DATA_TYPE'='DATA_TYPE') )

now()
#numDays<- audienceBBC %>%
       #group_by(ID) %>%
       #summarise(numDays = length(unique(as.Date(ymd_hms(audienceBBC$START))) ) )
now()

## get the average daily number of minutes per data type per person
bbcSplit<- audienceBBC_labelled %>%
  group_by(ID, WEEK,TYPE) %>%
  summarise(weeklyTimeSpent_sec = sum(as.numeric(hms(DURATION)))) %>%
  group_by(ID,TYPE)%>%
  summarise(dailyTimeSpent_min = mean(weeklyTimeSpent_sec)/(7*60))

bbcSplit$dailyTimeSpent_min[bbcSplit$dailyTimeSpent_min > 1440]<- 1440.0 #floor anything above 24 hours of time

bbcSplit<- bbcSplit %>%
  spread(TYPE, dailyTimeSpent_min) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(total_hrs =  (BBC_OD_RADIO
         +BBC_OD_TV
         +BBC_RADIO
         +BBC_TV
         +BBC_WEB
         +NOT_MARKET_WEB
         +OTHER_OD_TV
         +OTHER_OD_AUDIO
         +OTHER_RADIO
         +OTHER_TV
         +OTHER_WEB)/60)

now()

## join with metadata and weight
BBC_VFM<- inner_join(vfmAll, bbcSplit, by = 'ID') %>% select(4,1,2,3,5,6,7,8,9,10,11,12,13,14,15,16)
BBC_VFM<- inner_join(weightValue, BBC_VFM, by = 'ID')

## add age groupings
BBC_VFM_AGEBANDS <- BBC_VFM %>% 
  mutate(AGEGROUP = factor(cut(AGERAW, breaks = c(16,24,34,44,54,64,100), 
                               labels = c("16-24", "25-34","35-44","45-54", "55-64", "65+"))))%>%
  select(1,2,18,4,5,6,7,8,9,10,11,12,13,14,15,16,17) %>%
  distinct()
  
write.csv(BBC_VFM_AGEBANDS, "D:\\Projects\\VMF_Regression\\data\\BBC_VFM.csv",row.names = FALSE)
summary(BBC_VFM_AGEBANDS)


tempTimeSpent<-BBC_VFM_AGEBANDS %>% gather(key = "TYPE", value = "dailyTimeSpent_min",
                             BBC_OD_TV,
                             BBC_RADIO,
                             BBC_TV,
                             BBC_WEB,
                             NOT_MARKET_WEB,
                             OTHER_OD_TV,
                             OTHER_OD_AUDIO,
                             OTHER_RADIO,
                             OTHER_TV,
                             OTHER_WEB,
                             BBC_OD_RADIO)

#### plots #####
ggplot(data = BBC_VFM_AGEBANDS, mapping = aes(x = BBC_TV)) +
  geom_histogram(binwidth = 10)#+
  #scale_y_continuous(limits = c(0,10))

ggplot(data = tempTimeSpent, aes(y = dailyTimeSpent_min, x = TYPE))+
  geom_boxplot()+
  facet_wrap(~ TYPE, nrow = 1, scales = "free" )


### trim the data to set anything above the 95th percentile to the 95th percentile value ###
BBC_VFM_AGEBANDS<- as.data.frame(BBC_VFM_AGEBANDS)

trimData <- function(x){
  topLimit <- quantile( x, c(0.95 ))
  print(topLimit)
  x[ x < topLimit ] <- topLimit
}
for(col in 6:ncol(BBC_VFM_AGEBANDS)){trimData(BBC_VFM_AGEBANDS[,col])}

######### Normalise the data ##############
BBC_VFM_NORM<- as.data.frame(BBC_VFM_AGEBANDS)
BBC_VFM_NORM[6:17] <- apply(BBC_VFM_NORM[6:17], 2, scale)


#### check normalised data
tempTimeSpentNorm<-BBC_VFM_NORM %>% gather(key = "TYPE", value = "dailyTimeSpent_min",
                                           BBC_OD_TV,
                                           BBC_RADIO,
                                           BBC_TV,
                                           BBC_WEB,
                                           NOT_MARKET_WEB,
                                           OTHER_OD_TV,
                                           OTHER_OD_AUDIO,
                                           OTHER_RADIO,
                                           OTHER_TV,
                                           OTHER_WEB,
                                           BBC_OD_RADIO)
ggplot(data = tempTimeSpentNorm, aes(y = dailyTimeSpent_min, x = TYPE))+
  geom_boxplot()+
  facet_wrap(~ TYPE, nrow = 1, scales = "free" )


##############   Regression ########
fit1 <- lm(BBC_VMF ~ 
            AGEGROUP
          + GENDER
          + BBC_OD_RADIO
          + BBC_OD_TV
          + BBC_RADIO
          + BBC_TV
          + BBC_WEB
          + OTHER_OD_TV
          + OTHER_OD_AUDIO
          + OTHER_RADIO
          + OTHER_TV
          + OTHER_WEB
          ,
          data = BBC_VFM_NORM,
          weights = avgWeight)

summary(fit1) # show results

fit2 <- lm(BBC_VMF ~ 
             AGEGROUP
           + GENDER
           # + BBC_OD_RADIO
           # + BBC_OD_TV
           + BBC_RADIO
           + BBC_TV
           + BBC_WEB
           # + NOT_MARKET_WEB
            + OTHER_OD_TV
           # + OTHER_OD_AUDIO
           # + OTHER_RADIO
            + OTHER_TV
           # + OTHER_WEB
           ,
           data = BBC_VFM_NORM,
           weights = avgWeight)

summary(fit2) # show results


#### Give a training set of data and a testing set
trainingRows<- sample(1:nrow(BBC_VFM_NORM), 0.7*nrow(BBC_VFM_NORM))
trainingData<- BBC_VFM_NORM[trainingRows,]
testData<- BBC_VFM_NORM[-trainingRows,]

### build model on training data
lmModel<- lm(BBC_VMF ~ 
               AGEGROUP
             + GENDER
             + BBC_RADIO
             + BBC_TV
             + BBC_WEB
             + OTHER_OD_TV
             + OTHER_TV
             ,
             data = trainingData,
             weights = avgWeight
             )

summary(lmModel) # show results
