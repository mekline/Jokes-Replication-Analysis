#Relating behavioral and contrast data by subjects!


library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(bootstrap)

#(set your own wd first)
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
behavdir = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/behavioral_data/Jokes"

#New  - read in the nicely formatted behavioral data we made!
behavdata = read.csv(paste(behavdir, '/all_behavioral_output.csv', sep=''))

#Make sure you have AllSigChange!
View(allSigChange)

#We need to make sure to match up the right participants, so here we add the list order that participants
#were loaded into the ToM initial first-level analyses.

participants = c('168_FED_20161228b_3T2',
                 '290_FED_20170426a_3T2',
                 '301_FED_20161217b_3T2',
                 '366_FED_20161205a_3T2',
                 '426_FED_20161215c_3T2',
                 '430_FED_20170426d_3T2',
                 '498_FED_20170210c_3T2',
                 '555_FED_20170426c_3T2',
                 '576_FED_20170414b_3T2',
                 '577_FED_20170414c_3T2',
                 '578_FED_20170414d_3T2',
                 '288_FED_20170412b_3T2',
                 '334_FED_20161221a_3T2',
                 '343_FED_20161208a_3T2',
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
#(This drops any subjects who didn't get included for the Jokes analyses!)
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
cor(bb$meanResponseChange, bb$sigChange)

## Added an LM (no random slopes/intercepts! just 1 value/person)
m1 <- lm(sigChange ~ meanResponseChange, data = bb)
m0 <- lm(sigChange ~ 1, data = bb)
anova(m1,m0)

## MAKE PRETTY GRAPH
setwd("./figs")
coef(lm(meanResponseChange ~ sigChange, data = bb))

ggplot(data=bb, aes(y=sigChange, x=meanResponseChange)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.50), breaks = seq(-0.25, 0.50, 0.25)) + 
  scale_x_continuous(limits = c(0, 1.75), breaks = seq(0, 2, 0.5)) +
  xlab('average rating response \n(Jokes - Non-jokes)') +
  ylab('avg. % signal change \n(Jokes - Non-jokes)') +
  theme_bw() +
ggsave(filename="behav_activation.jpg", width=3, height=3)
  

######################
#Make the behavioral graphs for basic response times and ratings, (FIG 2)

# Drop NA response

behavdata <- filter(behavdata, !is.na(RT)) %>%
  filter(!is.na(response))

####
# RT
####
#Get average RTs per category per participant
avgRT <- behavdata %>%
  group_by(ID, category) %>%
  summarise(meanRT = mean(RT))

#T test
t.test(meanRT ~ category, data=avgRT)
#Response times are not different by condition


####
# Ratings
####

#Get average ratings per category per participant
behavdata$response <- as.numeric(as.character(behavdata$response))
avgResponse <- behavdata %>%
  group_by(ID, category) %>%
  summarise(meanResponse = mean(response))

t.test(meanResponse ~ category, data=avgResponse)
#Responses are different by condition! The jokes are funny!


####
# Graphs!
####

#sterr <- function(mylist){
#  my_se = sd(mylist)/sqrt(length(mylist)) 
#  
#  return(my_se)
#}

#Edit! We should be doing bootstrapped 95% confidence intervals instead! calculate them from allSigChange
#then merge into mystats

bootup <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean)
  return(quantile(foo$thetastar, 0.975)[1])
}
bootdown <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean)
  return(quantile(foo$thetastar, 0.025)[1])
}

#Make the organized data for ggplot
avgRT <- ungroup(avgRT)
avgResponse <- ungroup(avgResponse)

#plot millisecnds
avgRT$meanRT <- avgRT$meanRT * 1000
#rename categories
avgRT$categoryLabel <- ""
avgRT[avgRT$category == "joke",]$categoryLabel <- "Jokes"
avgRT[avgRT$category == "nonjoke",]$categoryLabel <- "Non-Jokes"
avgResponse$categoryLabel <- ""
avgResponse[avgResponse$category == "joke",]$categoryLabel <- "Jokes"
avgResponse[avgResponse$category == "nonjoke",]$categoryLabel <- "Non-Jokes"

toPlotRT = avgRT %>%
  group_by(categoryLabel)%>%
  summarise(mean = mean(meanRT))

tobootUp = avgRT %>%
  group_by(categoryLabel)%>%
  summarise(bootup = bootup(meanRT))
tobootDown = avgRT %>%
  group_by(categoryLabel)%>%
  summarise(bootdown = bootdown(meanRT))

#toPlotRT = merge(toPlotRT, toStr)
#toPlotRT$se_up <- toPlotRT$mean + toPlotRT$sterr
#toPlotRT$se_down <- toPlotRT$mean - toPlotRT$sterr
toPlotRT = merge(toPlotRT, tobootUp)
toPlotRT = merge(toPlotRT, tobootDown)

toPlotResp = avgResponse %>%
  group_by(categoryLabel)%>%
  summarise(mean = mean(meanResponse))

#toStr = avgResponse %>%
#  group_by(categoryLabel)%>%
#  summarise(sterrRes = sterr(meanResponse))

#toPlotResp = merge(toPlotResp, toStr)
#toPlotResp$se_up <- toPlotResp$mean + toPlotResp$sterr
#toPlotResp$se_down <- toPlotResp$mean - toPlotResp$sterr

tobootUp = avgResponse %>%
  group_by(categoryLabel)%>%
  summarise(bootup = bootup(meanResponse))
tobootDown = avgResponse %>%
  group_by(categoryLabel)%>%
  summarise(bootdown = bootdown(meanResponse))

toPlotResp = merge(toPlotResp, tobootUp)
toPlotResp = merge(toPlotResp, tobootDown)

setwd(mywd)

ggplot(data=toPlotRT, aes(y=mean, x=categoryLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,1200)) +
  scale_y_continuous(breaks = seq(0, 2000, 200))+
  xlab('Stimulus type') +
  ylab('Response time (milliseconds)') +
  scale_fill_manual(name="", values=c("gray35", "gray60")) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  # Optional, remove for RHLang and ToMCustom since we want the legend there...
  theme(legend.position="none")  
ggsave(filename="behavioralrt.jpg", width=3, height=3)

ggplot(data=toPlotResp, aes(y=mean, x=categoryLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(1,4)) +
  scale_y_continuous(breaks = seq(1, 4, 1))+
  xlab('Stimulus type') +
  ylab('Average funny-ness rating') +
  scale_fill_manual(name="", values=c("gray35", "gray60")) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  # Optional, remove for RHLang and ToMCustom since we want the legend there...
  theme(legend.position="none")  
ggsave(filename="behavioral.jpg", width=3, height=3)

