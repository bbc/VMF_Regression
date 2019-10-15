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
          GENDER,
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
          USAGE_OF_TV_SERVICES,
          ETHNICITY)
panelistsComplete<- panelistsComplete%>%
  filter(startsWith(panelistsComplete$INDIVIDUAL_ID, 'I') |startsWith(panelistsComplete$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>%
  group_by(ID)%>%
  mutate(AGE = round(mean(AGERAW),1)) %>%
  select(-INDIVIDUAL_ID, - WEIGHT, -AGERAW)%>% 
  distinct()
dbDisconnect(con)

#panellistColNames <- t(t(colnames(panelistsComplete)))
metadata<- inner_join(cluster %>% select(ID, cluster),
                       panelistsComplete %>% 
                        select(ID, AGE, GENDER, NATION, REGION, WORKING_STATUS, SOCIAL_GRADE, RELIGION, SEXUAL_ORIENTATION, ETHNICITY,
                               IMPRESSION_BBC, IMPRESSION_ITV, IMPRESSION_CHANNEL_4, IMPRESSION_CHANNEL_5,
                               BBC_VFM, SKY_VFM, VIRGIN_VFM),
                       by = "ID") %>%distinct()


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
metadata$IMPRESSION_BBC <- as.integer(as.character(metadata$IMPRESSION_BBC))
metadata$IMPRESSION_ITV <- as.integer(as.character(metadata$IMPRESSION_ITV))
metadata$IMPRESSION_CHANNEL_4 <- as.integer(as.character(metadata$IMPRESSION_CHANNEL_4))
metadata$IMPRESSION_CHANNEL_5 <- as.integer(as.character(metadata$IMPRESSION_CHANNEL_5))
metadata$BBC_VFM<- as.integer(as.character(metadata$BBC_VFM) )
metadata$SKY_VFM<- as.integer(as.character(metadata$SKY_VFM) )
metadata$VIRGIN_VFM<- as.integer(as.character(metadata$VIRGIN_VFM) )

head(metadata)


### compare the high VFM ranking cluster with the low one on their opinions of other channels.
tvOpinions_highVFMcluster <- metadata %>% 
  filter(cluster == 3) %>% 
  select(ID, 
         IMPRESSION_BBC, 
         IMPRESSION_ITV, 
         IMPRESSION_CHANNEL_4, 
         IMPRESSION_CHANNEL_5, 
         BBC_VFM, 
         SKY_VFM, 
         VIRGIN_VFM) %>% distinct()
tvOpinions_lowVFMcluster <- metadata %>% 
  filter(cluster == 4) %>% 
  select(ID, 
         IMPRESSION_BBC, 
         IMPRESSION_ITV, 
         IMPRESSION_CHANNEL_4, 
         IMPRESSION_CHANNEL_5, 
         BBC_VFM, 
         SKY_VFM, 
         VIRGIN_VFM)%>% distinct()


write.csv(tvOpinions_highVFMcluster, "D:\\Projects\\VMF_Regression\\data\\ClusterAnalysis\\tvOpinions_highVFMcluster.csv", row.names = FALSE)
summary(tvOpinions_highVFMcluster)
summary(tvOpinions_lowVFMcluster)


############################### Compare the groups on their metadata ############################
meta_highVFMcluster <- metadata %>% 
  filter(cluster == 3) %>% 
  select(ID, 
         AGE,
         NATION,
         REGION,
         WORKING_STATUS,
         SOCIAL_GRADE,
         RELIGION,
         SEXUAL_ORIENTATION,
         ETHNICITY
         ) %>% distinct() 

meta_lowVFMcluster <- metadata %>% 
  filter(cluster == 4) %>% 
  select(ID, 
         AGE,
         NATION,
         REGION,
         WORKING_STATUS,
         SOCIAL_GRADE,
         RELIGION,
         SEXUAL_ORIENTATION,
         ETHNICITY
  ) %>% distinct()

summary(meta_highVFMcluster)
summary(meta_lowVFMcluster)
write.csv(meta_lowVFMcluster, "D:\\Projects\\VMF_Regression\\data\\ClusterAnalysis\\meta_lowVFMcluster.csv", row.names = FALSE)


#### summarise meta data with proportions from each section
colNames <- t(t(colnames(meta_highVFMcluster))) ##create a list of the column names

### for high VFM group
metaSummary_highVFMcluster<- data.frame()
for(col in 3:ncol(meta_highVFMcluster)){
  temp<- meta_highVFMcluster %>% 
    group_by_at(col) %>%
    summarise(perc = round(100*length(unique(ID))/964,1)) %>%
    mutate(category = as.character(colNames[col]))
  colnames(temp)[1]<- "subcategory"
  
  temp<- temp %>% select(3,1,2)
  
  metaSummary_highVFMcluster<- rbind(metaSummary_highVFMcluster, temp)
}

write.csv(metaSummary_highVFMcluster, "D:\\Projects\\VMF_Regression\\data\\ClusterAnalysis\\metaSummary_highVFMcluster.csv", row.names = FALSE)

### for low vfm group
colNames <- t(t(colnames(meta_lowVFMcluster))) ##create a list of the column names
metaSummary_lowVFMcluster<- data.frame()

for(col in 3:ncol(meta_lowVFMcluster)){
  temp<- meta_lowVFMcluster %>% 
    group_by_at(col) %>%
    summarise(perc = round(100*length(unique(ID))/852,1)) %>%
    mutate(category = as.character(colNames[col]))
  colnames(temp)[1]<- "subcategory"
  
  temp<- temp %>% select(3,1,2)
  
  metaSummary_lowVFMcluster<- rbind(metaSummary_lowVFMcluster, temp)
}

write.csv(metaSummary_lowVFMcluster, "D:\\Projects\\VMF_Regression\\data\\ClusterAnalysis\\metaSummary_lowVFMcluster.csv", row.names = FALSE)

 ######################   Gender Age ##############################

summary(metadata %>% ### low group
           filter(cluster == 4) %>% 
           select(ID, 
                  AGE,
                  GENDER
           ) %>% distinct()
)

summary(metadata %>% ### high group
          filter(cluster == 3) %>% 
          select(ID, 
                 AGE,
                 GENDER
          ) %>% distinct()
)
