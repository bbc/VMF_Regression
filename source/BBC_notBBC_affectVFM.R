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

audienceBBC_labelled %>% filter(as.numeric(hms(DURATION)) < 180)


## get the average daily number of minutes per data type per person
### remove any duration under 3 minutes
bbcSplit<- audienceBBC_labelled %>% 
  filter(as.numeric(hms(DURATION)) > 180) %>%
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
  topLimit <<- quantile( x, c(0.95 ))
  print(topLimit)
  #print(x[ x > topLimit ])# <- topLimit
  return(topLimit)
}

for(col in 6:ncol(BBC_VFM_AGEBANDS)){
  print(col)
  trimData(BBC_VFM_AGEBANDS[,col])
  for(row in 1:nrow(BBC_VFM_AGEBANDS)){ 
    if(BBC_VFM_AGEBANDS[row,col] > topLimit){BBC_VFM_AGEBANDS[row,col]<- topLimit}
    }
  }

write.csv(BBC_VFM_AGEBANDS, "D:\\Projects\\VMF_Regression\\data\\BBC_VFM_trimmed.csv",row.names = FALSE)
temp1<- BBC_VFM_AGEBANDS %>% 
  group_by(ID) %>% 
  mutate(TOTAL_BBC = BBC_OD_RADIO + BBC_OD_TV + BBC_RADIO + BBC_TV + BBC_WEB)



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
          #+ BBC_OD_RADIO remove as all values are zero because 95% of data is 0 minutes
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
testData$BBC_VMF[testData$BBC_VMF <1]<- 0.001 ## remove any zeros for the MAPE


  
### build model on training data
lmModel<- lm(BBC_VMF ~ 
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
             data = trainingData,
             weights = avgWeight
             )

summary(lmModel) # show results
AIC(lmModel)

vfmPrediction<- predict(lmModel, testData)### predict the VFM on the test data

### predict the VFM and compare to known ####
vfmPredictActual<- data.frame(cbind(actuals=testData$BBC_VMF, predicteds = vfmPrediction)) ## actual VFM and that predicted in one df

correlation_accuracy <- cor(vfmPredictActual)  ## correlation

ggplot(data = vfmPredictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()
ggplot(data = vfmPredictActual, mapping = aes(y = actuals, x = predicteds))+
  geom_point()


min_max_acuracy <- mean(apply(vfmPredictActual, 1, min) / apply(vfmPredictActual, 1, max)) ## min_max accuracy
mape <- mean(abs((vfmPredictActual$predicteds - vfmPredictActual$actuals))/vfmPredictActual$actuals) ##mean absolute percentage deviation

#library('DMwR')



DMwR::regr.eval(vfmPredictActual$actuals, vfmPredictActual$predicteds)



#### k- fold cross validation ##### using k samples to create k models and then averaging over them
library(DAAG)
cvResults <- suppressWarnings(CVlm( data = BBC_VFM_NORM, 
                                    form.lm = BBC_VMF ~ 
                                      AGEGROUP
                                    + GENDER
                                    + BBC_RADIO
                                    + BBC_TV
                                    + BBC_WEB
                                    + OTHER_OD_TV
                                    + OTHER_TV, 
                                    #weights = avgWeight,
                                    m=5, 
                                    dots=FALSE, 
                                    seed=29, 
                                    legend.pos="topleft",  
                                    printit=FALSE, 
                                    main="Small symbols are predicted values while bigger ones are actuals."))  # performs the CV
attr(cvResults, 'ms') 


###### looking for correlated values ####
ggplot(data = BBC_VFM_NORM, mapping = aes(x = BBC_TV, y = BBC_RADIO))+
  geom_point()

pairs(~ BBC_TV+ BBC_RADIO +BBC_WEB + BBC_OD_TV + BBC_OD_RADIO, 
      data = BBC_VFM_AGEBANDS
      )

library(hexbin)

plot( hexbin(BBC_VFM_AGEBANDS$BBC_RADIO, BBC_VFM_AGEBANDS$OTHER_RADIO, xbins = 50),
      main="Hexagonal Binning" )


############## LM with split by gender ######################

trainingDataMEN<- trainingData %>% filter(GENDER ==1)
testDataMEN<- testData %>% filter(GENDER ==1)

lmModelMen<- lm(BBC_VMF ~ 
                   AGEGROUP
                 + BBC_RADIO
                 + BBC_TV
                 + BBC_WEB
                 + OTHER_OD_TV
                 + OTHER_TV
                 ,
                 data = trainingDataMEN,
                 weights = avgWeight
)
summary(lmModelMen)
prediction<- predict(lmModelMen, testDataMEN)
predictActual<- data.frame(cbind(actuals=testDataMEN$BBC_VMF, predicteds = prediction))
cor(predictActual)

ggplot(data = predictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()

##########
trainingDataWOMEN<- trainingData %>% filter(GENDER ==2)
testDataWOMEN<- testData %>% filter(GENDER ==2)

lmModelWomen<- lm(BBC_VMF ~ 
                  AGEGROUP
                + BBC_RADIO
                + BBC_TV
                + BBC_WEB
                + OTHER_OD_TV
                + OTHER_TV
                ,
                data = trainingDataWOMEN,
                weights = avgWeight
)
summary(lmModelWomen)
prediction<- predict(lmModelWomen, testDataWOMEN)
predictActual<- data.frame(cbind(actuals=testDataWOMEN$BBC_VMF, predicteds = prediction))
cor(predictActual)

ggplot(data = predictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()

####################  AGE SPLITS ###############

trainingDataAGE<- trainingData %>% filter(AGEGROUP == '25-34')
testDataAGE<- testData %>% filter(AGEGROUP == '16-24')

lmModelAGE<- lm(BBC_VMF ~ 
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
                  data = trainingDataAGE,
                  weights = avgWeight
)
summary(lmModelAGE)
prediction<- predict(lmModelAGE, testDataAGE)

predictActual<- data.frame(cbind(actuals=testDataAGE$BBC_VMF, predicteds = prediction))
cor(predictActual)

ggplot(data = predictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()

####################  Light VS Heavy Users ###############
usageLevel<- BBC_VFM_AGEBANDS %>% 
  group_by(ID) %>% 
  mutate(TOTAL_BBC = BBC_OD_RADIO + BBC_OD_TV + BBC_RADIO + BBC_TV + BBC_WEB)
summary(usageLevel$TOTAL_BBC)

usageLight <- usageLevel %>% filter(TOTAL_BBC <= 29.56)
usageMed <- usageLevel %>% filter(TOTAL_BBC >= 29.56 & TOTAL_BBC<= 120.43)

usageNorm <- as.data.frame(usageMed)
usageNorm[6:17] <- apply(usageNorm[6:17], 2, scale)

trainingRows<- sample(1:nrow(usageNorm), 0.7*nrow(usageNorm))
trainingData<- usageNorm[trainingRows,]
testData<- usageNorm[-trainingRows,]

lmModelUSAGE<- lm(BBC_VMF ~ 
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
                data = trainingData,
                weights = avgWeight
)
summary(lmModelUSAGE)
prediction<- predict(lmModelUSAGE, testData)

predictActual<- data.frame(cbind(actuals=testData$BBC_VMF, predicteds = prediction))
cor(predictActual)

ggplot(data = predictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()
#######################

predictMean<- data.frame(cbind(actuals=testDataAGE$BBC_VMF, predicteds = mode(trainingDataAGE$BBC_VMF)))
cor(predictMean) 
ggplot(data = predictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()


############## LM with percentage time ######################
percTimeSpent<- tempTimeSpent %>% 
  mutate(percTime_min = 100*dailyTimeSpent_min/(total_hrs*60)) %>%
  select(-dailyTimeSpent_min) %>%
  spread(TYPE, percTime_min)


trainingDataPerc<- percTimeSpent[trainingRows,]
testDataPerc<- percTimeSpent[-trainingRows,]

lmModelPerc<- lm(BBC_VMF ~ 
               AGEGROUP
             + GENDER
             + BBC_RADIO
             + BBC_TV
             + BBC_WEB
             + OTHER_OD_TV
             + OTHER_TV
             ,
             data = trainingDataPerc,
             weights = avgWeight
)
summary(lmModelPerc)
percPrediction<- predict(lmModelPerc, testDataPerc)
percPredictActual<- data.frame(cbind(actuals=testDataPerc$BBC_VMF, predicteds = percPrediction))
cor(percPredictActual)

ggplot(data = percPredictActual, mapping = aes(x = actuals, y = predicteds))+
  geom_point()



