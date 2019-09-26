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



## Read in and manipulate time viewing data #####

timeSpent <- read.csv("D:\\Projects\\VMF_Regression\\data\\avgTimeSpentBBC.csv", header = TRUE)
timeSpent$INDIVIDUAL_ID<- as.character(timeSpent$INDIVIDUAL_ID)
timeSpent$STREAM_LABEL<- as.character(timeSpent$STREAM_LABEL)

for(row in 1:nrow(timeSpent)){
  if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC1')){timeSpent$STREAM_LABEL[row] = "BBC1"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC2')){timeSpent$STREAM_LABEL[row] = "BBC2"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC4')){timeSpent$STREAM_LABEL[row] = "BBC4"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC News')){timeSpent$STREAM_LABEL[row] = "NEWS"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC Alba')){timeSpent$STREAM_LABEL[row] = "ALBA"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'BBC Parliament')){timeSpent$STREAM_LABEL[row] = "PARLIAMENT"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'CBeebies')){timeSpent$STREAM_LABEL[row] = "CBEEBIES"}
  else if(startsWith(timeSpent$STREAM_LABEL[row], 'CBBC')){timeSpent$STREAM_LABEL[row] = "CBBC"}
  else {print(paste0("unknown - ",timeSpent$STREAM_LABEL[row]))}
}

numRemoved = 0
numRows<- nrow(timeSpent)

for(row in 2:numRows){
  if(timeSpent[row,1] == timeSpent[row-1,1] & timeSpent[row,2] == timeSpent[row-1,2]){
    print(paste0("Match - Row ", row, " and row ", row-1))
    timeSpent[row, 3] <- timeSpent[row-1,3 ] + timeSpent[row,3 ]
    print(timeSpent[row, 3])
    timeSpent<-timeSpent[-(row-1), ]
    numRemoved<-numRemoved+1
  }
  if(row == numRows-numRemoved){break}
}

### In percentage format
timeSpentPer<- timeSpent %>%
  group_by(INDIVIDUAL_ID) %>%
  mutate(total = sum(AVG_DURATION_SEC))%>%
  mutate(durationPerc = round(100*AVG_DURATION_SEC/total,1))%>%
  select(-AVG_DURATION_SEC, -total)%>%
  spread(STREAM_LABEL, durationPerc, fill = 0)

## In raw seconds format
timeSpentRaw<- timeSpent %>%spread(STREAM_LABEL, AVG_DURATION_SEC , fill = 0)

#### bring in value for money data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
vfm <- collect(tbl(con, 'panellstsVMF'))
dbDisconnect(con)


### Join percentage usage of each channel with VMF
timeSpentPerVFM<-left_join(vfm, timeSpentPer, by = "INDIVIDUAL_ID")
timeSpentRawVFM<-left_join(vfm, timeSpentRaw, by = "INDIVIDUAL_ID")


## Some people have no BBC TV recorded
## List these people
IDs_noBBC <- timeSpentRawVFM %>%filter(is.na(ALBA))%>% select(INDIVIDUAL_ID)

## Join to the activity file
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
panellistsAll <- collect(tbl(con, 'panelistsAll12Weeks'))
audienceBehaviour<- collect(tbl(con, 'audienceData12Weeks')) ## beaviour not on web
dbDisconnect(con)

behaviour_noBBC<- left_join(IDs_noBBC, audienceBehaviour, by = 'INDIVIDUAL_ID')
userActivityLevel <- behaviour_noBBC %>%
  filter(!is.na(STREAM_LABEL)) %>% ## remove IDs with no behaviour recorded
  filter(str_detect(STREAM_LABEL,"Radio", negate = TRUE)) %>% ## remove radio behaviour to see only tv
  group_by(INDIVIDUAL_ID)%>%
  summarise(numStreamLabels = length(unique(STREAM_LABEL)))

## list people on IDs_noBBC but not on userActivityLevel list - these will be removed.
removeIDs<- anti_join(IDs_noBBC, userActivityLevel, by = "INDIVIDUAL_ID")

## remove people from timeSpentVFM
timeSpentPerVFM<- anti_join(timeSpentPerVFM,removeIDs, by = "INDIVIDUAL_ID")
timeSpentPerVFM[is.na(timeSpentPerVFM)]<-0

timeSpentRawVFM<- anti_join(timeSpentRawVFM,removeIDs, by = "INDIVIDUAL_ID")
timeSpentRawVFM[is.na(timeSpentRawVFM)]<-0

write.csv(timeSpentPerVFM, file = "D:\\Projects\\VMF_Regression\\data\\timeSpentPerVFM.csv", row.names = FALSE)
write.csv(timeSpentRawVFM, file = "D:\\Projects\\VMF_Regression\\data\\timeSpentRawVFM.csv", row.names = FALSE)


### Repeat but not include people who viewed no channels and remove people who only viewed one channel.
timeSpent
singleUsers<-timeSpent %>% 
  group_by(INDIVIDUAL_ID) %>% 
  filter(length(unique(STREAM_LABEL))==1) %>%
  select(INDIVIDUAL_ID)
timeSpent2 <- anti_join(timeSpent, singleUsers) 



timeSpent2Per<- timeSpent2 %>%
  group_by(INDIVIDUAL_ID) %>%
  mutate(total = sum(AVG_DURATION_SEC))%>%
  mutate(durationPerc = round(100*AVG_DURATION_SEC/total,1))%>%
  select(-AVG_DURATION_SEC, -total)%>%
  spread(STREAM_LABEL, durationPerc, fill = 0)


timeSpent2PerVFM<-inner_join(vfm, timeSpent2Per, by = "INDIVIDUAL_ID")
write.csv(timeSpent2PerVFM, file = "D:\\Projects\\VMF_Regression\\data\\timeSpent2PerVFM.csv", row.names = FALSE)                  

### People who've viewed 3 or more channels
singleUsers<-timeSpent %>% 
  group_by(INDIVIDUAL_ID) %>% 
  filter(length(unique(STREAM_LABEL))<3) %>%
  select(INDIVIDUAL_ID)
timeSpent2 <- anti_join(timeSpent, singleUsers)

test<- timeSpent2 %>% group_by(INDIVIDUAL_ID) %>%mutate(num = length(unique(STREAM_LABEL)))



timeSpent2Per<- timeSpent2 %>%
  group_by(INDIVIDUAL_ID) %>%
  mutate(total = sum(AVG_DURATION_SEC))%>%
  mutate(durationPerc = round(100*AVG_DURATION_SEC/total,1))%>%
  select(-AVG_DURATION_SEC, -total)%>%
  spread(STREAM_LABEL, durationPerc, fill = 0)


timeSpent2PerVFM<-inner_join(vfm, timeSpent2Per, by = "INDIVIDUAL_ID")
write.csv(timeSpent2PerVFM, file = "D:\\Projects\\VMF_Regression\\data\\timeSpent2PerVFM.csv", row.names = FALSE)  
