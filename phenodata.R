library(tidyverse)
library(lubridate)

HFDSph <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p00/hf003/hf003-04-fall.csv"))

HFDSph <-
  HFDSph %>%
  filter(grepl("^Q", tree.id)) %>%
  select("date", "doy", "tree.id", "lcolor", "comments")

HFDSdc <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv"))

HFDSdc <-
  HFDSdc %>%
  select("date", "temp")

HFDScomb <-
  HFDSph %>%
  left_join(HFDSdc %>% select(date, temp), by=c("date"=))


