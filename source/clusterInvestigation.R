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

## read in people and which cluster they were assigned to.
cluster <- read.csv("D:\\Projects\\VMF_Regression\\data\\numPlatformClusteredGroups.csv", header = TRUE)
cluster <-  cluster %>% distinct()

### bring in panelists data conversions from Nation = 1 to Nation = England
panelistsCodes <- read.csv("D:\\Projects\\compassData\\panelistsCodes.csv", header = TRUE) %>%
  select(-QUESTION)
panelistsCodes$CODE<- as.integer(as.character(panelistsCodes$CODE))



#### bring in panelists data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
panelistsComplete <- collect(tbl(con, 'panelistsComplete')) %>% 
  filter(WEEK <= 8,WEEK >= 5)%>%
  select( INDIVIDUAL_ID,
          WEIGHT,
          AGERAW,
          NATION,
          REGION,
          BBC_REGIONS,
          ITV_REGIONS,
          GOR_REGIONS,
          WORKING_STATUS,
          SOCIAL_GRADE,
          TV_RECEPTION_MAIN_TV,
          TV_RECEPTION_OTHER_TV,
          PVR_MAIN_TV,
          ACCESS_TO_TV_SERVICES,
          USAGE_OF_TV_SERVICES,
          ONLINE_ACCOUNT,
          IMPRESSION_BBC,
          IMPRESSION_ITV,
          IMPRESSION_CHANNEL_4,
          IMPRESSION_CHANNEL_5,
          IMPRESSION_SKY,
          BBC_VFM,
          SKY_VFM,
          VIRGIN_VFM,
          SOCIAL_GRADE_2, 
          RELIGION, 
          SEXUAL_ORIENTATION, 
          MARITAL_STATUS, 
          USAGE_OF_TV_SERVICES)
panelistsComplete<- panelistsComplete%>%
  filter(startsWith(panelistsComplete$INDIVIDUAL_ID, 'I') |startsWith(panelistsComplete$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>%
  select(-INDIVIDUAL_ID, - WEIGHT)%>% 
  distinct()
dbDisconnect(con)

#panellistColNames <- t(t(colnames(panelistsComplete)))
metadata<- inner_join(cluster %>% select(ID, cluster),
                       panelistsComplete %>% 
                        select(ID, AGERAW, NATION, REGION, WORKING_STATUS, SOCIAL_GRADE, RELIGION, SEXUAL_ORIENTATION, MARITAL_STATUS,
                               IMPRESSION_BBC, IMPRESSION_ITV, IMPRESSION_CHANNEL_4, IMPRESSION_CHANNEL_5,
                               BBC_VFM, SKY_VFM, VIRGIN_VFM),
                       by = "ID")


metadataColNames <- t(t(colnames(metadata))) ##create a list of the column names in metadata
## loop across the columns, join with the relavent part of the code df and replace the code with the names.
for(x in 4:ncol(metadata)){
  print(colnames(metadata)[4])
  colnames(metadata)[4]<- "CODE"
  
  metadata<- left_join(metadata, 
            panelistsCodes %>% 
              filter(PANELLIST_FILE_NAME == metadataColNames[x]) %>% 
              select(-PANELLIST_FILE_NAME),
            by = "CODE")
  
  
  colnames(metadata)[ncol(metadata)]<- metadataColNames[x]
  metadata <- metadata %>% select(-CODE)
}
head(metadata)


