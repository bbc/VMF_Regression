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

streamLabels <- read.csv("D:\\Projects\\VMF_Regression\\StreamLabels.csv", header = TRUE)


#### bring in data from sqlite database
dbPath <- ("D:\\Projects\\CMMData.db")
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
vfm1 <- collect(tbl(con, 'panellstsVMF'))
temp <- tbl(con, 'audienceData12Weeks_incWeb')
audience<- collect(filter(temp, WEEK <= 8,
                  WEEK >= 8))
dbDisconnect(con)

audienceBBC<- audience %>% 
  filter(startsWith(audience$INDIVIDUAL_ID, 'I') |startsWith(audience$INDIVIDUAL_ID, 'H') )%>% 
  mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>%
  distinct()
audienceBBC<- inner_join(audienceBBC, streamLabels, by = 'STREAM_LABEL')

vfm <- vfm1 %>%mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>% 
  filter(AGERAW <35) %>% 
  select(ID, BBC_VMF) %>% 
  distinct()

vfmAll <- vfm1 %>%mutate(ID = substr(INDIVIDUAL_ID, 3,11)) %>% 
  select(-INDIVIDUAL_ID) %>% 
  filter(AGERAW <35) %>%
  distinct()


BBC_VFM<- inner_join(audienceBBC, vfm, by = 'ID')
view(BBC_VFM %>% filter(ID == '1000098_1')%>%group_by(ID, WEEK))

view(BBC_VFM %>% group_by(ID, WEEK) %>%
       summarise(totalDur = sum(as.numeric(hms(DURATION)))))

allBBC <- BBC_VFM %>% group_by(ID, WEEK) %>%
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(ID) %>%
 summarise(avgDuration = round(mean(totalDur)/(7*3600),1))

allBBC<- inner_join(allBBC, vfm) 

my.formula <- y ~ x
ggplot(data = allBBC, mapping = aes(x = avgDuration, y = BBC_VMF)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_classic() +
  labs(title = "BBC VFM vs. Avg Time Spent", x = "Average Hours With BBC Per Day", y = "BBC VFM",
       subtitle = "16-34", caption = "Compass: W5-8 2019")




write.csv(allBBC, file = "D:\\Projects\\VMF_Regression\\data\\allBBC.csv", row.names = FALSE)


tooManyHours<- allBBC %>%filter(avgDuration > 5)


view(audienceBBC %>% filter(ID =='1007677_1'))



radio <- BBC_VFM %>% filter(TYPE == 'RADIO') %>%
 group_by(ID, WEEK) %>%
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(ID) %>%
  summarise(avgDuration = round(mean(totalDur)/(7*3600),1))
radio <- inner_join(radio, vfm)

tv <- BBC_VFM %>% filter(TYPE == 'TV') %>%
  group_by(ID, WEEK) %>%
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(ID) %>%
  summarise(avgDuration = round(mean(totalDur)/(7*3600),1))
tv <- inner_join(tv, vfm)

online <- BBC_VFM %>% filter(TYPE == 'WEB') %>%
  group_by(ID, WEEK) %>%
  summarise(totalDur = sum(as.numeric(hms(DURATION)))) %>%
  group_by(ID) %>%
  summarise(avgDuration = round(mean(totalDur)/(7*3600),2))
online <- inner_join(online, vfm)

radioGraph<- ggplot(data = radio, mapping = aes(x = avgDuration, y = BBC_VMF)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_classic() +
  labs(title = "BBC VFM vs. Avg Time Spent RADIO", x = "Average Hours With BBC Per Day", y = "BBC VFM",
       subtitle = "16-34", caption = "Compass: W5-8 2019")
radioGraph


tvGraph<- ggplot(data = tv, mapping = aes(x = avgDuration, y = BBC_VMF)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_classic() +
  labs(title = "BBC VFM vs. Avg Time Spent TV", x = "Average Hours With BBC Per Day", y = "BBC VFM",
       subtitle = "16-34", caption = "Compass: W5-8 2019")
tvGraph

onlineGraph<-ggplot(data = online, mapping = aes(x = avgDuration, y = BBC_VMF)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE)+
  theme_classic() +
  labs(title = "BBC VFM vs. Avg Time Spent ONLINE", x = "Average Hours With BBC Per Day", y = "BBC VFM",
       subtitle = "16-34", caption = "Compass: W5-8 2019")
onlineGraph


###########################   Box Plots ##########################

summary(tv)


ggplot(data = inner_join(allBBC,vfmAll%>%select(ID,GENDER)) , aes(x = as.factor(GENDER), y = avgDuration))+
  geom_boxplot(aes(fill= as.factor(GENDER)))+
  ylim(0,2)+
  geom_hline(yintercept = 0.25,linetype="dashed", color = "red")+
 geom_text(aes( 0, 0.25, label = 0.25, vjust = -1, hjust = -0.5), size = 3)+
  theme_classic()+
  labs(title = "BBC VFM vs. Avg Time Spent All", x = "BBC VFM",y = "Average Hours With BBC Per Day" ,
       subtitle = "16-34", caption = "Compass: W5-8 2019")+
  facet_wrap(~ BBC_VMF, nrow = 1)


ggplot(data = allBBC, aes(x = as.factor(BBC_VMF), y = avgDuration ))+
  geom_boxplot()+
  ylim(0,4)+
  geom_hline(yintercept = 0.4,linetype="dashed", color = "red")+
  geom_text(aes( 0, 0.4, label = 0.4, vjust = -1, hjust = -0.5), size = 3)+
  theme_classic()+
  labs(title = "BBC VFM vs. Avg Time Spent All", x = "BBC VFM",y = "Average Hours With BBC Per Day" ,
       subtitle = "16-34", caption = "Compass: W5-8 2019")

ggplot(data = tv, aes(x = as.factor(BBC_VMF), y = avgDuration ))+
  geom_boxplot()+
  ylim(0,2)+
  geom_hline(yintercept = 0.25,linetype="dashed", color = "red")+
  geom_text(aes( 0, 0.25, label = 0.25, vjust = -1, hjust = -0.5), size = 3)+
  theme_classic()+
  labs(title = "BBC VFM vs. Avg Time Spent TV", x = "BBC VFM",y = "Average Hours With BBC Per Day" ,
       subtitle = "16-34", caption = "Compass: W5-8 2019")


ggplot(data = radio, aes(x = as.factor(BBC_VMF), y = avgDuration ))+
  geom_boxplot()+
  ylim(0,2)+
  geom_hline(yintercept = 0.25,linetype="dashed", color = "red")+
  geom_text(aes( 0, 0.25, label = 0.25, vjust = -1, hjust = -0.5), size = 3)+
  theme_classic()+
  labs(title = "BBC VFM vs. Avg Time Spent RADIO", x = "BBC VFM",y = "Average Hours With BBC Per Day" ,
       subtitle = "16-34", caption = "Compass: W5-8 2019")

ggplot(data = online, aes(x = as.factor(BBC_VMF), y = avgDuration ))+
  geom_boxplot()+
  ylim(0,0.15)+
  geom_text(aes( 0, 0.01, label = 0.01, vjust = -1, hjust = -0.5), size = 3)+
  geom_hline(yintercept = 0.01,linetype="dashed", color = "red")+
  theme_classic()+
  labs(title = "BBC VFM vs. Avg Time Spent ONLINE", x = "BBC VFM",y = "Average Hours With BBC Per Day" ,
       subtitle = "16-34", caption = "Compass: W5-8 2019")
  
