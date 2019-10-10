### to get the average time people spend in one week with each TV channel

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

## read in labels recorded by compass and wether they relate to bbc TV
streamLabels <- read.csv("D:\\Projects\\VMF_Regression\\tvLabels.csv", header = TRUE)


#### bring in data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
#vfm1 <- collect(tbl(con, 'panellstsVMF'))
temp <- tbl(con, 'audienceData12Weeks_incWeb')
audience<- collect(filter(temp, WEEK <= 8,
                          WEEK >= 5))
audienceWeight <- collect(filter(tbl(con, 'panelistsAll12WeeksWEIGHT'), 
                                 WEEK <= 8,
                                 WEEK >= 5))
dbDisconnect(con)


##Simplify the ID for if it needs to be joined to other dbs
audienceBBC<- audience %>% 
  filter(startsWith(audience$INDIVIDUAL_ID, 'I') |startsWith(audience$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>%
  distinct()

##use only TV with time of each session greater than 3 minutes
timeSpentTV <- audienceBBC %>% 
  inner_join(streamLabels %>% filter(TYPE == 'TV'), by = "STREAM_LABEL") %>%
  select(ID, WEEK, DURATION, LABEL)%>%
  filter(as.numeric(hms(DURATION)) > 180)


## get the duration per week for each label then average, any label not used set to zero.
timeSpentTV<- timeSpentTV %>% 
  mutate(duration_sec = as.numeric(hms(DURATION)))%>% 
  group_by(ID, WEEK, LABEL) %>%
    summarise(weeklyDuration_sec = sum(duration_sec)) %>%
  group_by(ID, LABEL) %>%
  summarise(avgDailyDuration_min = mean(weeklyDuration_sec)/(7*60))%>%
  spread(key = LABEL, value = avgDailyDuration_min ) %>%
  mutate_all(~replace(., is.na(.), 0.0001)) %>%
  mutate(totalDailyTime_min = ONE + TWO + FOUR +CBBC + CBEEBIES + NEWS +PARLIAMENT)

write.csv(timeSpentTV, file = "D:\\Projects\\VMF_Regression\\data\\timeSpentTV.csv", row.names = FALSE)
  
  