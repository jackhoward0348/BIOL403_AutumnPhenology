library(tidyverse)
library(lubridate)
library(dplyr)

HFDSph <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p00/hf003/hf003-04-fall.csv"))

HFDSph <-
  HFDSph %>%
  filter(grepl("^Q", tree.id)) %>%
  select("date", "doy", "tree.id", "lcolor", "comments")

#Shaver site up through 2001
HFDSdcSh <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv"))


HFDSdcSh <-
  HFDSdcSh %>%
  select("date", "airt") %>%
  filter(date > "1991-01-01") %>%
  filter(date < "2001-02-11")

#Fischer site from 2001-present
HFDSdcFischer <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-06-daily-m.csv"))

HFDSdcFischer <-
  HFDSdcFischer %>%
  select("date", "airt") %>%
  filter(date < "2019-11-21")

HFDSdcCOMB <- rbind(HFDSdcFischer, HFDSdcSh)



#comb doesn't have Fischer data yet
HFDSdata <-
  HFDSph %>%
  left_join(HFDSdcCOMB %>% select(date, airt), by=c("date"="date"))


HFDSdata$date <- as.character(HFDSdata$date)

HFDSdcCOMB<-
  HFDSdcCOMB %>%
  separate(date, c("year", "month", "day"), sep = "-")

HFDSdcCOMB <-
  HFDSdcCOMB %>%
  filter(month %in% c("08", "09", "10"))
  
HFDSdcAnalysis <- aggregate(HFDSdcCOMB$airt, list(HFDSdcCOMB$year), mean, na.rm=TRUE)

colnames(HFDSdcAnalysis) <- c("Year", "airt")

HFDSdcAnalysis$Year <- as.numeric(HFDSdcAnalysis$Year)

#Fig 1: temperature over time
ggplot(data = HFDSdcAnalysis, aes(x=Year, y= airt, color = airt)) +
  geom_point() +
  ylim(13.5,17) +
  ylab("Average Air Temperature (C)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  geom_smooth(level= 0.90) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#wrangle to get 90 coloration
HFDSdata1 <- HFDSdata %>%
  filter(lcolor > 89)

HFDSdata1$year <- as.numeric(HFDSdata1$year)

test1 <- 
  HFDSdata1 %>%
  group_by(year, tree.id) %>%
  slice_min(n = 1, doy)


test1<-
  test1 %>%
  separate(tree.id, c("species", "ID"), sep = "-")

rename(plottrees)


plottrees <- aggregate(test1$doy, list(test1$year, test1$species), mean, na.rm=TRUE)
colnames(plottrees) <- c("year", "species", "averageDOY")


ggplot(data = plottrees, aes(x=year, y=averageDOY, color = species)) +
  geom_point() +
  geom_smooth(level= 0.80) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("Average Day of Year of Full Coloration") +
  xlab("Year") 
  
  
  
 
  
  





