library(data.table)
library(zoo)
library(tidyverse)
library(here)
library(rio)
library(tcltk)
library(haven)
library(reshape2)
library(janitor)
library(magrittr)
library(ggrepel)
library(fontawesome)

#### Potential Plot 3, First Draft

pupilData <- import(here("data","pupils binned by block FINAL.sav"),setclass="tbl_df")
pupilData <- read_sav("Data/pupils binned by block FINAL.sav")
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
plotData4 <- ddply(plotData, .(Subject), mutate, bin = seq_along(variable.y))

oop <- ddply(plotData4, c("value.x", "bin"), summarise,
             N=length(!is.na(value.y)),
             mean=mean(value.y),
             sd=sd(value.y),
             se=sd/sqrt(N)) 

write_sav(oop2, path = paste(dir, "/", "vis3dataTEST.sav", sep = ""))
oop2 <- oop[1:597,]
oopPlot <- oop2 %>% 
  ggplot( aes(x=bin, y=mean, group=value.x, color=value.x)) +
  geom_line() +
  geom_point() +
  ggtitle("Pupil Response by Bin") +
  theme_ipsum() +
  ylab("Pupil Diameter") +
  theme(legend.position="none") +
  transition_reveal(bin)

  animate(oopPlot, height = 4,
        width = 4, units = "in", res = 150)
  anim_save("pupil by time bin.gif")


#################################################

# First draft of Plot 1

# Data
behav <- read_sav("Data/eye4crt Ranked agg.sav")

behav$condition <- as.factor(behav$condition)
behav1 <- behav %>%
  gather(key = "Rank", value = "BinRT", meanRank1, meanRank2, meanRank3, meanRank4, meanRank5)

behavt <- behav1 %>%
  group_by(Rank) %>%
  mutate(meanRT = mean(BinRT))

behav2 <- behav1 %>%
  pivot_wider(names_from = Rank, values_from = BinRT)

tg <- ddply(behav1, c("condition", "Rank"), summarise,
            N=length(!is.na(BinRT)),
            mean=mean(BinRT),
            sd=sd(BinRT),
            se=sd/sqrt(N)) 

tg[2] <- as.numeric(c("1","2","3","4","5","1","2","3","4","5"))
tg <- as.data.frame(tg)
tg[1] <- ifelse(tg$condition==1, "non-specific goal","harder-over-time goal")
dir <- tclvalue(tkchooseDirectory())
write_sav(tg, path = paste(dir, "/", "vis1data.sav", sep = ""))

# v1.1
plot11t <- behavt %>%
  group_by(condition,Rank) %>%
  ggplot(aes(x=Rank, y=BinRT, group=BinRT, color= condition)) +
  geom_point() +
  geom_line(aes(color=condition)) +
  theme_bw() +
#  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition")
plot11t

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


plot13 <- ggplot(data=tg, aes(x=Rank, y=mean, color= condition, linetype=condition, label=condition)) +
  geom_point(size=4) +
  geom_line(aes(linetype=condition, color=condition),size=2) +
  theme_bw() +
  labs(x = "Bin", 
       y = "Response Time (in milliseconds)",
       color="Condition", 
       linetype="Condition", 
       title="Goal-Setting Reduces Attention Lapses",
       subtitle="Response Time Distribution on a 4-Choice Response Time Task",
       caption="Note: Bin = response times rank-ordered smallest to largest (1 = fastest, 5 = slowest).
       The graph shows that those with no-goal display significantly longer slow RTs.") +
  scale_color_manual(name= "Condition", labels = c("No-Goal", "Goal"), values = c("dark green", "black")) +
  scale_linetype_manual(name="Condition", labels = c("No-Goal", "Goal"), values=c(1,4)) +
  geom_label_repel(data = filter(tg, Rank==5), 
                   aes(label = condition),
                   nudge_x = -.6,
                   segment.size  = 0.3,
                   segment.color = "grey50",
                   direction     = "x",
                   na.rm = TRUE) +
  theme_minimal() +
  theme(legend.position="none") 
plot13


second_to_last_quarter <- max(tg$Rank[tgf$Rank != max(tg$Rank)])


#######################################

# First Draft of Plot 2







