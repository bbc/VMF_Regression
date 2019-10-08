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

## only select people in the metadata

pillarsData<- inner_join(audienceBBC, pillars, by = "STREAM_LABEL")

numPillars<- pillarsData %>% 
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
plot(h, colramp= function(n){heat.ob(n,beg=256,end=40)}) 

## there is some relationship, more platforms gives higher vfm
