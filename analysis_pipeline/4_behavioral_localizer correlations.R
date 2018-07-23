#This set of analyses is for relating (individual) behavioral and contrast data by subjects. 
rm(list = ls())
library(bootstrap)
library(dplyr)
library(ggplot2)
library(lme4)
library(pwr)
library(stringr)
library(tidyr)

#(set your own wd first)
behavdir = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/E2_behavioral_data/Jokes"
analysisdir = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline"
setwd(analysisdir)

#New  - read in the nicely formatted behavioral data we made!
behavdata = read.csv(paste(behavdir, '/all_behavioral_output.csv', sep=''))

#Make sure you have AllSigChange!
load("allSigChange.RData")

####
# Ratings
####

#Get average ratings per category per participant
behavdata$response <- as.numeric(as.character(behavdata$response))
jokeResponseChange <- behavdata %>%
  mutate(participantID = ID)%>%
  filter(!is.na(response)) %>% 
  group_by(participantID, category) %>%
  summarise(meanResponse = mean(response)) %>%
  spread(category, meanResponse) %>%
  mutate(meanResponseChange = joke-nonjoke)

####
# Signal change
####

jokeSigChange <- allSigChange %>%
  filter(contrastName == 'joke-lit', ROIMask == 'ToM', localizer == 'ToM', task == 'Jokes', ROIName == 'LocalizerAverage')

#Merge the datasets!
bb <- merge(jokeResponseChange, jokeSigChange, by=c('participantID'))

## REPORT STATS
#E2 result - these are not highly correlated. 
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
ggsave(filename="behav_activation_E2.jpg", width=3, height=3)
  
#Calculate and save a version of the E2 behavdata so we can make composite graphs in that script!
nrow(behavdata)
behavdata <- filter(behavdata, !is.na(RT)) %>%
  filter(!is.na(response))
nrow(behavdata)
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

setwd(analysisdir)
save(avgRT, file = "avgRT.RData")
save(avgResponse, file = "avgResponse.RData")

# ####
# # Graphs!
# ####
# 
# #sterr <- function(mylist){
# #  my_se = sd(mylist)/sqrt(length(mylist)) 
# #  
# #  return(my_se)
# #}
# 
# #Edit! We should be doing bootstrapped 95% confidence intervals instead! calculate them from allSigChange
# #then merge into mystats
# 
# bootup <- function(mylist){
#   foo <- bootstrap(mylist, 1000, mean)
#   return(quantile(foo$thetastar, 0.975)[1])
# }
# bootdown <- function(mylist){
#   foo <- bootstrap(mylist, 1000, mean)
#   return(quantile(foo$thetastar, 0.025)[1])
# }
# 
# #Make the organized data for ggplot
# avgRT <- ungroup(avgRT)
# avgResponse <- ungroup(avgResponse)
# 
# #plot millisecnds
# avgRT$meanRT <- avgRT$meanRT * 1000
# #rename categories
# avgRT$categoryLabel <- ""
# avgRT[avgRT$category == "joke",]$categoryLabel <- "Jokes"
# avgRT[avgRT$category == "nonjoke",]$categoryLabel <- "Non-Jokes"
# avgResponse$categoryLabel <- ""
# avgResponse[avgResponse$category == "joke",]$categoryLabel <- "Jokes"
# avgResponse[avgResponse$category == "nonjoke",]$categoryLabel <- "Non-Jokes"
# 
# toPlotRT = avgRT %>%
#   group_by(categoryLabel)%>%
#   summarise(mean = mean(meanRT))
# 
# tobootUp = avgRT %>%
#   group_by(categoryLabel)%>%
#   summarise(bootup = bootup(meanRT))
# tobootDown = avgRT %>%
#   group_by(categoryLabel)%>%
#   summarise(bootdown = bootdown(meanRT))
# 
# #toPlotRT = merge(toPlotRT, toStr)
# #toPlotRT$se_up <- toPlotRT$mean + toPlotRT$sterr
# #toPlotRT$se_down <- toPlotRT$mean - toPlotRT$sterr
# toPlotRT = merge(toPlotRT, tobootUp)
# toPlotRT = merge(toPlotRT, tobootDown)
# 
# toPlotResp = avgResponse %>%
#   group_by(categoryLabel)%>%
#   summarise(mean = mean(meanResponse))
# 
# #toStr = avgResponse %>%
# #  group_by(categoryLabel)%>%
# #  summarise(sterrRes = sterr(meanResponse))
# 
# #toPlotResp = merge(toPlotResp, toStr)
# #toPlotResp$se_up <- toPlotResp$mean + toPlotResp$sterr
# #toPlotResp$se_down <- toPlotResp$mean - toPlotResp$sterr
# 
# tobootUp = avgResponse %>%
#   group_by(categoryLabel)%>%
#   summarise(bootup = bootup(meanResponse))
# tobootDown = avgResponse %>%
#   group_by(categoryLabel)%>%
#   summarise(bootdown = bootdown(meanResponse))
# 
# toPlotResp = merge(toPlotResp, tobootUp)
# toPlotResp = merge(toPlotResp, tobootDown)
# 
# setwd("./figs")
# 
# ggplot(data=toPlotRT, aes(y=mean, x=categoryLabel)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
#   coord_cartesian(ylim=c(0,1200)) +
#   scale_y_continuous(breaks = seq(0, 2000, 200))+
#   xlab('Stimulus type') +
#   ylab('Response time (milliseconds)') +
#   scale_fill_manual(name="", values=c("gray35", "gray60")) +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(strip.background = element_blank()) +
#   # Optional, remove for RHLang and ToMCustom since we want the legend there...
#   theme(legend.position="none")  
# ggsave(filename="behavioralrt.jpg", width=3, height=3)
# 
# ggplot(data=toPlotResp, aes(y=mean, x=categoryLabel)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
#   coord_cartesian(ylim=c(1,4)) +
#   scale_y_continuous(breaks = seq(1, 4, 1))+
#   xlab('Stimulus type') +
#   ylab('Average funny-ness rating') +
#   scale_fill_manual(name="", values=c("gray35", "gray60")) +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(strip.background = element_blank()) +
#   # Optional, remove for RHLang and ToMCustom since we want the legend there...
#   theme(legend.position="none")  
# ggsave(filename="behavioral.jpg", width=3, height=3)