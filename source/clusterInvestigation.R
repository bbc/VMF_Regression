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
cluster <- read.csv("D:\\Projects\\VMF_Regression\\data\\numPlatformClusteredGroups.csv", header = TRUE)

#### bring in data from sqlite database
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
          SOCIAL_GRADE_2) %>% distinct()
dbDisconnect(con)
