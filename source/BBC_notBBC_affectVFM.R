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

streamLabels <- read.csv("D:\\Projects\\VMF_Regression\\ALL_STREAM_LABELS.csv", header = TRUE)

#### bring in data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
vfm1 <- collect(tbl(con, 'panellstsVMF'))
temp <- tbl(con, 'audienceData12Weeks_incWeb')
audience<- collect(filter(temp, WEEK <= 8,
                          WEEK >= 5))
dbDisconnect(con)


audienceBBC<- audience %>% 
  filter(startsWith(audience$INDIVIDUAL_ID, 'I') |startsWith(audience$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>%
  distinct()

vfmAll <- vfm1 %>%mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID, -SKY_VMF, -VIRGIN_VMF) %>%
  distinct()

audienceBBC <- inner_join(vfmAll %>%select(ID),audienceBBC,  by = 'ID' )

audienceBBC_labelled<- inner_join(audienceBBC, streamLabels, by = c('STREAM_LABEL' = 'STREAM_LABEL', 'DATA_TYPE'='DATA_TYPE') )
now()
numDays<- audienceBBC %>%
       group_by(ID) %>%
       summarise(numDays = length(unique(as.Date(ymd_hms(audienceBBC$START))) ) )
now()

bbcSplit<- audienceBBC_labelled %>%
  group_by(ID, TYPE) %>%
  summarise(timeSpent_sec = sum(as.numeric(hms(DURATION)))) %>%
  spread(TYPE, timeSpent_sec) %>%
  mutate_all(~replace(., is.na(.), 0))

BBC_VFM<- inner_join(vfmAll, bbcSplit, by = 'ID') %>% select(4,1,2,3,5,6,7,8,9,10,11,12,13,14,15)

write.csv(BBC_VFM, "D:\\Projects\\VMF_Regression\\data\\BBC_VFM.csv",row.names = FALSE)

