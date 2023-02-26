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

pupilDataOop <- plotData4 %>%
  group_by(value.x, variable.y) %>%
  summarize(meanEye = mean(value.y),
            bin2 = mean(bin))
  

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

oop <- ddply(plotData4, c("value.x", "bin"), summarise,
             N=length(!is.na(value.y)),
             mean=mean(value.y),
             sd=sd(value.y),
             se=sd/sqrt(N)) 

x<-rep(c(1:100),times=6)

oop[7] <- x

oopF <- ddply(oop, c("value.x", "V7"), summarise,
             N=length(!is.na(mean)),
             mean=mean(mean),
             sd=sd(mean),
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
oopPlot

oopPlotF <- oopF %>% 
  ggplot( aes(x=V7, y=mean, group=value.x, color=value.x)) +
  geom_line() +
  geom_point() +
  ggtitle("Pupil Response by Bin") +
  theme_ipsum() +
  ylab("Pupil Diameter") +
  theme(legend.position="none") +
  transition_reveal(V7)
oopPlotF

  animate(oopPlotF, height = 4,
        width = 4, units = "in", res = 150)
  anim_save("pupil by time bin.gif")

  oopPlotF2 <- oopF %>% 
    ggplot( aes(x=V7, y=mean, group=as.factor(value.x), color=as.factor(value.x))) +
    geom_line(aes(lty = as.factor(value.x))) +
    geom_point() +
    ggtitle("Pupil Size Change by Bin") +
    ylab("Pupil Diameter") +
    labs(x = "Time Bin", y = "Pupil Diamemter (in arbitrary units)",
         color="Condition",
         title="Goal-Setting Increases Attentional Effort",
         subtitle="Pupil Dilation after Target-Onset by Condition",) +
    scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
    scale_linetype_manual(values = c("1" = "dashed", # assign line types to groups
                                     "2" = "solid"),
                          guide = FALSE) +
    theme_minimal()+
    theme(legend.position="none") +
    transition_reveal(V7)
  oopPlotF2
  
  animate(oopPlotF2, height = 4,
          width = 4, units = "in", res = 150)
  anim_save("pupil by time bin 2.gif")
  
  
  oopPlotF3 <- oopF %>% 
    ggplot( aes(x=V7, y=mean, group=as.factor(value.x), color=as.factor(value.x))) +
    geom_line(aes(lty = as.factor(value.x))) +
    geom_point() +
    ggtitle("Pupil Size Change by Bin") +
    ylab("Pupil Diameter") +
    labs(x = "Time Bin", y = "Pupil Diamemter (in arbitrary units)",
         color="Condition",
         title="Goal-Setting Increases Attentional Effort",
         subtitle="Pupil Dilation after Target-Onset by Condition",) +
    scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("#56B4E9", "#CC79A7")) +
    scale_linetype_manual(values = c("1" = "dashed", # assign line types to groups
                                     "2" = "solid"),
                          guide = FALSE) +
    theme_minimal()+
    theme(legend.position="none") +
    transition_reveal(V7)
  oopPlotF3
  
  animate(oopPlotF3, height = 4,
          width = 4, units = "in", res = 150)
  anim_save("pupil by time bin 3.gif")
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

p2data <- behav

dir <- tclvalue(tkchooseDirectory())
write_sav(p2data, path = paste(dir, "/", "vis2data.sav", sep = ""))

plot21 <- ggplot(data=p2data, aes(x=condition, y=meanRT)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1))
plot21

plot22 <- ggplot(data=p2data, aes(x=condition, y=meanRT, color=condition)) +
  geom_jitter() +
  theme_bw() +
  labs(x = "Bin", y = "Response Time (in milliseconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1)) +
  theme_minimal()
plot22


p2data2 <- p2data %>% 
  group_by(condition) %>% 
  summarize(mean = mean(meanRT))
write_sav(p2data2, path = paste(dir, "/", "vis2data2.sav", sep = ""))

plot23 <- ggplot(data=p2data, aes(x=condition, y=meanRT, color=condition)) +
  geom_jitter(width = 0.25, alpha = 0.5, size = 3) +
  theme_bw() +
  labs(x = "Condition", y = "Average Response Time (in milliseconds)",
       color="Condition", linetype="Condition",
       title="Goal-Setting Improves Task Performance",
       subtitle="Average Response Times by Condition during 4-ChoiceRT") +
  scale_x_discrete(labels=c("1" = "No-Goal", "2" = "Harder-over-time Goal")) +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("#F0E442", "#CC79A7")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1)) +
  theme_minimal() +
  geom_segment(data = filter(p2data2, condition == "1"), aes(x = 0.75, xend = 1.25, y = mean, yend = mean), linetype = "dashed", size = .75, color = "black") + 
  geom_segment(data = filter(p2data2, condition == "2"), aes(x = 1.75, xend = 2.25, y = mean, yend = mean), linetype = "dashed", size = .75, color = "black") + 
  theme(legend.position="none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5))
plot23

