library(data.table)
library(zoo)
library(tidyverse)
library(here)
library(rio)
library(tcltk)
library(haven)


eyeData <- import(here("data","Eye4CRT Behavioral SPSS for EDLD Project.sav"),setclass="tbl_df")
  
eyeData2 <- eyeData %>% 
  arrange(Subject, totalCorrectRTs) %>%
  group_by(Subject) %>% 
  mutate(rank = rank(totalCorrectRTs, na.last = 'keep')) %>%
  mutate(rt_Bin = cut(rank, breaks = 5, labels = seq(1,5))) %>%
  mutate(meanRT = mean(totalCorrectRTs, na.rm=T)) %>%
  mutate(sdRT = sd(totalCorrectRTs, na.rm=T)) %>%
  mutate(lapseCutoff = meanRT*2) %>%
  mutate(YNlapse = ifelse(totalCorrectRTs > lapseCutoff, 1, 0)) %>%
  mutate(ProbeB1 = ifelse(Running == 'Triallist', probe.RESP, NA)) %>%
  mutate(ProbeB2 = ifelse(Running == 'TrialList2', probe.RESP, NA)) %>%
  mutate(ProbeB3 = ifelse(Running == 'TrialList3', probe.RESP, NA)) %>%
  mutate(TUTB1 = sum(ProbeB1 >=3, na.rm = T)) %>%
  mutate(TUTB2 = sum(ProbeB2 >=3, na.rm = T)) %>%
  mutate(TUTB3 = sum(ProbeB3 >=3, na.rm = T)) %>%
  mutate(B1propOFF = TUTB1/5, na.rm=T) %>%
  mutate(B2propOFF = TUTB2/5, na.rm=T) %>%
  mutate(B3propOFF = TUTB3/5, na.rm=T)


summary <- eyeData2 %>%
  group_by(Subject) %>% 
  summarize(Subject = first(Subject),
            condition = first(ExperimentName),
            sumCorrectRT = sum(totalCorrectRTs >=0, na.rm = T),
            meanRT = mean(totalCorrectRTs, na.rm = T),
            meanACC = mean(totalACC_mean, na.rm = T),
            lapseSum = sum(totalCorrectRTs>lapseCutoff, na.rm = T),
            sumTUT = sum(probe.RESP>=3, na.rm = T),
            propTUT = sumTUT/15,
            
            meanRank1 = mean(ifelse(rt_Bin == 1, totalCorrectRTs, NA), na.rm = T),
            meanRank2 = mean(ifelse(rt_Bin == 2, totalCorrectRTs, NA), na.rm = T),
            meanRank3 = mean(ifelse(rt_Bin == 3, totalCorrectRTs, NA), na.rm = T),
            meanRank4 = mean(ifelse(rt_Bin == 4, totalCorrectRTs, NA), na.rm = T),
            meanRank5 = mean(ifelse(rt_Bin == 5, totalCorrectRTs, NA), na.rm = T),
            
            rtBlock1 = mean(ifelse(Running == 'Triallist', totalCorrectRTs, NA), na.rm = T),
            rtBlock2 = mean(ifelse(Running == 'TrialList2', totalCorrectRTs, NA), na.rm = T),
            rtBlock3 = mean(ifelse(Running == 'TrialList3', totalCorrectRTs, NA), na.rm = T),
            
            lapseBlock1 = sum(ifelse(Running == 'Triallist', YNlapse, NA), na.rm=T),
            lapseBlock2 = sum(ifelse(Running == 'TrialList2', YNlapse, NA), na.rm=T),
            lapseBlock3 = sum(ifelse(Running == 'TrialList3', YNlapse, NA), na.rm=T),
            
            TUTBlock1 = mean(TUTB1),
            TUTBlock2 = mean(TUTB2),
            TUTBlock3 = mean(TUTB3)
  )

summary[2] <- 0

summary <- as.data.table(summary)


dir <- tclvalue(tkchooseDirectory())
write_sav(summary, path = paste(dir, "/", "eye4crt Ranked agg.sav", sep = ""))
