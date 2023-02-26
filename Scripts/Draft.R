library(data.table)
library(zoo)
library(tidyverse)
library(here)
library(rio)
library(tcltk)
library(haven)
library(reshape2)

#### Potential Plot 3, First Draft

pupilData <- import(here("data","pupils binned by block FINAL.sav"),setclass="tbl_df")
pupilData2 <- pupilData[,-2]

# rearrange dataset
pupilData <- melt(pupilData, id=c("Subject"))
pupilData2 <- melt(pupilData2, id=c("Subject"))
pupilData3 <- pupilData[1:81,]
plotData <- left_join(pupilData3, pupilData2, "Subject", multiple = "all")

# 

library(gganimate)
library(hrbrthemes)
library(plyr) 
plotData2 <- ddply(plotData, .(Subject), mutate, bin = seq_along(variable.y))

plotData2 %>%
  ggplot( aes(x=bin, y=value.y, group=value.x, color=value.x)) +
  geom_line() +
  geom_point() +
  ggtitle("Pupil Response by Bin") +
  theme_ipsum() +
  ylab("Pupil Diameter") +
  transition_reveal(bin)







