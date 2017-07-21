#Relating behavioral and contrast data by subjects!


library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)

#(set your own wd first)
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
behavdir = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/behavioral_data/Jokes"

#New  - read in the nicely formatted behavioral data we made!
behavdata = read.csv(paste(behavdir, '/all_behavioral_output.csv', sep=''))

#Make sure you have AllSigChange!
View(allSigChange)

#We need to make sure to match up the right participants, so here we add the list order that participants
#were loaded into the ToM initial first-level analyses.

participants = c('168_KAN_evDB_20141020b',
                 '290_FED_20170109b_3T2',
                 '301_FED_20150708c_3T2',
                 '366_FED_20161103a_3T1',
                 '426_FED_20161107c_3T2',
                 '430_FED_20170110b_3T2',
                 '498_FED_20170210c_3T2',
                 '555_FED_20170426c_3T2',
                 '576_FED_20170414b_3T2',
                 '577_FED_20170414c_3T2',
                 '578_FED_20170414d_3T2',
                 '288_FED_20160411a_3T1',
                 '334_FED_20160204c_3T2',
                 '343_FED_20160204b_3T2',
                 '521_FED_20161228a_3T2',
                 '551_FED_20170412a_3T2',
                 '571_FED_20170412c_3T2',
                 '473_FED_20170210b_3T2',
                 '520_FED_20161227a_3T2',
                 '596_FED_20170426b_3T2')

participants = as.data.frame(participants)
participants$SubjectNumber = 1:nrow(participants)
participants$ID = participants$participant

allSigChange <- merge(allSigChange, participants, by=c('SubjectNumber'), all_x=TRUE, all_y=TRUE)
#(This drops subjects who didn't get included for the Jokes analyses!)
####
# Ratings
####

#Get average ratings per category per participant
behavdata$response <- as.numeric(as.character(behavdata$response))
jokeResponseChange <- behavdata %>%
  filter(!is.na(response)) %>% 
  group_by(ID, category) %>%
  summarise(meanResponse = mean(response)) %>%
  spread(category, meanResponse) %>%
  mutate(meanResponseChange = joke-nonjoke)

####
# Signal change
####

jokeSigChange <- allSigChange %>%
  filter(contrastName == 'joke-lit', Group == 'ToM', task == 'Jokes', ROIName == 'LocalizerAverage')

#Merge the datasets!
bb <- merge(jokeResponseChange, jokeSigChange, by=c('ID'))

## REPORT STATS
cor(bb$meanResponseChange, bb$meanSigChange)

## Added an LM (no random slopes/intercepts! just 1 value/person)
m1 <- lm(meanSigChange ~ meanResponseChange, data = bb)
m0 <- lm(meanSigChange ~ 1, data = bb)
anova(m1,m0)

## MAKE PRETTY GRAPH
setwd("~/Dropbox/_Projects/Jokes - fMRI/Jokes-Analysis Repository/Analyses_paper/reproducible analyses/figs")
coef(lm(meanResponseChange ~ meanSigChange, data = bb))

ggplot(data=bb, aes(y=meanSigChange, x=meanResponseChange)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.50), breaks = seq(-0.25, 0.50, 0.25)) + 
  scale_x_continuous(limits = c(0, 1.75), breaks = seq(0, 2, 0.5)) +
  xlab('average rating response \n(Jokes - Non-jokes)') +
  ylab('avg. % signal change \n(Jokes - Non-jokes)') +
  theme_bw() +
ggsave(filename="behav_activation.jpg", width=3, height=3)
  
