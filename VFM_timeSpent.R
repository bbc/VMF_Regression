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

streamLabels <- read.csv("D:\\Projects\\VMF_Regression\\StreamLabels.csv", header = TRUE)


#### bring in data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
vfm <- collect(tbl(con, 'panellstsVMF'))
audience<- collect(tbl(con, 'audienceData12Weeks_incWeb'))
dbDisconnect(con)

audienceBBC<- inner_join(audience, streamLabels, by = 'STREAM_LABEL')
audienceBBC<- audienceBBC %>% filter(startsWith(audienceBBC$INDIVIDUAL_ID, 'I') |startsWith(audienceBBC$INDIVIDUAL_ID, 'H') )

radio <- audienceBBC %>% filter(TYPE == 'RADIO') %>% 
  group_by(INDIVIDUAL_ID, WEEK) %>% 
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(INDIVIDUAL_ID) %>%
  summarise(avgDuration = mean(totalDur))

tv <- audienceBBC %>% filter(TYPE == 'TV') %>% 
  group_by(INDIVIDUAL_ID, WEEK) %>% 
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(INDIVIDUAL_ID) %>%
  summarise(avgDuration = mean(totalDur))

online <- audienceBBC %>% filter(TYPE == 'WEB') %>% 
  group_by(INDIVIDUAL_ID, WEEK) %>% 
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(INDIVIDUAL_ID) %>%
  summarise(avgDuration = mean(totalDur))

radioVFM <-inner_join(radio, vfm) 
tvVFM <-inner_join(tv, vfm )
onlineVFM <-inner_join(online, vfm ) 

write.csv(radioVFM, file = "D:\\Projects\\VMF_Regression\\data\\radioVFM.csv", row.names = FALSE)
write.csv(tvVFM, file = "D:\\Projects\\VMF_Regression\\data\\tvVFM.csv", row.names = FALSE)
write.csv(onlineVFM, file = "D:\\Projects\\VMF_Regression\\data\\onlineVFM.csv", row.names = FALSE)
