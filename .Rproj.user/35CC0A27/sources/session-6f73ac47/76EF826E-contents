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
  transition_reveal(bin) +
  anim_save("pupil by time bin.gif")


#################################################

# First draft of Plot 1

# Data
behav <- read_sav("Data/eye4crt Ranked agg.sav")

behav$condition <- as.factor(behav$condition)
behav1 <- behav %>%
  gather(key = "Rank", value = "BinRT", meanRank1, meanRank2, meanRank3, meanRank4, meanRank5)

behav2 <- behav1 %>%
  pivot_wider(names_from = Rank, values_from = BinRT)

tg <- ddply(behav1, c("condition", "Rank"), summarise,
            N=length(!is.na(BinRT)),
            mean=mean(BinRT),
            sd=sd(BinRT),
            se=sd/sqrt(N)) 

tg[2] <- as.numeric(c("1","2","3","4","5","1","2","3","4","5"))
tg <- as.data.frame(tg)
dir <- tclvalue(tkchooseDirectory())
write_sav(tg, path = paste(dir, "/", "vis1data.sav", sep = ""))

# v1.1
plot11 <- ggplot(data=tg, aes(x=Rank, y=mean, color= condition)) +
  geom_point() +
  geom_line(aes(color=condition)) +
  theme_bw() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition")
plot11


# v1.2
plot12 <- ggplot(data=tg, aes(x=Rank, y=mean, color= condition, linetype=condition)) +
  geom_point() +
  geom_line(aes(linetype=condition, color=condition)) +
  theme_bw() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1))
plot12


# v1.3
plot13 <- ggplot(data=tg, aes(x=Rank, y=mean, color= condition, linetype=condition)) +
  geom_point() +
  geom_line(aes(linetype=condition, color=condition)) +
  theme_bw() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1)) +
  theme_minimal()
plot13







