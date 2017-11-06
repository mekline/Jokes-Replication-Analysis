#EXPLORATORY ANALYSES LIVE HERE.  

#We recapitulate the cleaning/processing code here just to avoid infecting the main scripts with e.g. 
#the split-half parts of the data. Interesting stuff starts on line 93.

#Load libraries
rm(list = ls())
library(bootstrap)
library(dplyr)
library(lme4)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)

bootup <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean)
  return(quantile(foo$thetastar, 0.975)[1])
}
bootdown <- function(mylist){
  foo <- bootstrap(mylist, 1000, mean)
  return(quantile(foo$thetastar, 0.025)[1])
}

setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
meansig_outputs_folder = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/E2_meansignal_outputs/'

###########
#(((EXPLORATORY A - Extend the high-med-low individual joke rating tests to the other systems. Appears in Supplemental E2)))
##########
#after powering the study up for the replication, we now detect (probably smaller) significant effects
#in all systems for jokes > nonjokes. The ToM ones are > RHLang and RMD (good!) but not significantly different in magnitude to 
#Lang or MDL.  One way to show that those MD and RHL activations are tapping something other than humor in the task would be if 
#funniness ratings didn't correlate with activation strength.  Let's see! (Oh wait, pause, this requires running more first level
#analyses to get those contrasts.  Check with Ev first. )
#(In fact, funniness ratings do correlate with activations in these regions as well)

#Load all the t tests (from E2)
allTests <- read.csv('localizer_t_tests_all.csv')
allTests <- allTests %>%
  mutate(sig = p < 0.05) %>%
  mutate(sigCor = p.adj < 0.05) %>%
  mutate(mismatch = sig != sigCor)

#STOP HAMMER TIME Load the full result set for all signal changes (by running file 2 thru 121)
View(allSigChange)

#Convention: when all tests go one way, report them together as follows:
reportTests <- function(ts, ps){
  if (all(ps > 0.05)){
    paste('all insig, ts <', max(ts), 'ps>', min(ps))
  } else if (all(ps < 0.05)){
    paste('all sig, ts >', min(ts), 'ps<', max(ps))
  } else {
    'explore...'
  }
}

#Extend the paramfun contrasts of the critical task to measure them in lang and in MD!
allTests %>%
  filter(Group == 'RHLang', task == 'JokesCustom', contrastName == 'linear') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'LHLang', task == 'JokesCustom', contrastName == 'linear') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'MDRight', task == 'JokesCustom', contrastName == 'linear') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'MDLeft', task == 'JokesCustom', contrastName == 'linear') %>%
  summarise(n(), sum(sig), reportTests(t,p))

#Now the same, with LME for all parcels in the localizers


RHLCustom <- filter(allSigChange, Group == "RHLang", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
RHLCustom$contrastName <- as.factor(RHLCustom$contrastName)
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLCustom)
anova(m1,m0)

LHLCustom <- filter(allSigChange, Group == "LHLang", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
LHLCustom$contrastName <- as.factor(LHLCustom$contrastName)
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLCustom)
anova(m1,m0)

MDRCustom <- filter(allSigChange, Group == "MDRight", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
MDRCustom$contrastName <- as.factor(MDRCustom$contrastName)
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRCustom)
anova(m1,m0)

MDLCustom <- filter(allSigChange, Group == "MDLeft", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
MDLCustom$contrastName <- as.factor(MDLCustom$contrastName)
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLCustom)
anova(m1,m0)


###########
#(((EXPLORATORY B - Checking for behaviorally 'oddball' subjects. Supplemental E1)))
##########

# There are some differences between studies 1 and 2! In particular, we see overall 
# bumps in activation (J > NJ) for the other 2 systems, Lang and MD.
# One thing Ev wondered was whether there were any 'oddball' responders in our
# task who were evaluating the jokes very differently.  To determine this, we're going 
# to try and calculate a value for each person, which is: "how far away from the mean
# response is this person's average answer"

#Prereq: run script 5behavioral to reload behavioral data and 2figs to load the signal-change data!
View(behavdata)
View(allSigChange)

#(((EXPLORATORY D, C is below now)))
# Make a table that aggregates responses by *item* (not person)
avgItemResponse <- behavdata %>%
  group_by(item, category) %>%
  summarise(meanResponse = mean(response, na.rm=TRUE))

#Merge it back to the main table
behavdata <- merge(behavdata, avgItemResponse, by=c("item","category"))

#Add distance-from-mean, and do a summary table!
oddballSubj <- behavdata %>%
  mutate(distanceFromMean = response - meanResponse) %>%
  group_by(ID, category) %>%
  summarize(myMeanDistance = mean(distanceFromMean, na.rm=TRUE))


#Visualize that....
ggplot(data=oddballSubj, aes(y=myMeanDistance, x=category)) + 
  geom_point(stat = "identity")

#Is there anyone who is an outlier? Do a boxplot to see
# (Details: ) The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.

ggplot(data=oddballSubj, aes(y=myMeanDistance, x=category)) + 
  geom_boxplot(stat = "boxplot") +
  xlab('') +
  ylab('Average distance from other subjects\' ratings')

#Interpretation: there are two people who found the NONjokes a bit funnier than we might expect, e.g. ~0.5 points funnier than the average person, that's it. 
#(This is about half the observed effect size, jokes are about 1 point funnier than nonjokes over the whole dataset)

#########
# (((EXPLORATORY C - Appears in Supplemental section E1)))
#########

#FROM EV:
#get the average joke>non-joke effect size from Study 1 vs. Study 2, 
#and correlate these. I'd be curious to see correlations
#* within each of the 5 sets of fROIs,
#* across all fROIs in the first three sets
#* across all fROIs

#This would give us a sense of how consistent the relative sizes of the 
#effects across systems and fROIs are across studies.

#NOTE view mystats and check you have the version with Experiment 1 values in it too. If not, run the script with the composite graphs (7composite... to get it)

jokelits <- mystats %>%
  filter(contrastName == 'joke-lit') %>%
  select(c(GroupLabel, ROIName, Experiment, themean)) %>%
  filter(ROIName != "average\nacross\nfROIs") %>%
  mutate(Experiment = ifelse(Experiment == "Experiment 1", 'Experiment1', 'Experiment2')) %>%
  spread(Experiment, themean) %>%
  filter(ROIName != "VM\nPFC") %>% #VMPFC was dropped from E1 therefore from these comparisons 
  mutate(ROIName_noN = gsub("\n","", ROIName))

cor_labels <- jokelits %>%
  group_by(GroupLabel) %>%
  summarize(group_cor = cor(Experiment1, Experiment2, method="spearman")) %>%
  mutate(my_cor_label = paste("\u03C1=", round(group_cor, 3)))


setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline/figs")
#A Graph
library(ggrepel)
ggplot(data=jokelits, aes(y=Experiment2, x=Experiment1, color = GroupLabel)) + 
  facet_wrap(~ GroupLabel, ncol=3, scales = "free") +
  geom_smooth(method="lm", se=FALSE) + 
  geom_point() +
  expand_limits(x = 0, y = 0) +
  xlab('Joke>Non-Joke signal change, \nExperiment 1') +
  ylab('Joke>Non-Joke signal change, \nExperiment 2') +
  geom_text_repel(aes(label = ROIName_noN), box.padding = unit(0.4, "lines"),
                  size = 3, color="black") +
  geom_text(data=cor_labels, aes(label=my_cor_label), 
            x=Inf, y=-Inf, hjust=1, vjust=-0.6,
            colour="black", inherit.aes=FALSE, parse=FALSE) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggsave(filename="compare_activation_E1_E2.jpg", width=9, height=6)

# START HERE, PUT R VALUES ON GRAPH!!!! 

cor(jokelits$Experiment1, jokelits$Experiment2, method="spearman")


#Takehome: activations within each system are relatively well correlated with 1 another. 

#########
#########

#Exploratory analysis: How do the signal changes for Joke > NonJoke compare to the localizer signal change in each 
#ROI? Are these *proportions* different for the different signals? 

localizer2task <- allSigChange %>%
  filter(contrastName %in% c('joke-lit','H-E','S-N','bel-pho')) %>%
  filter(task != 'JokesCustom') %>%
  filter(ROIName != 'LocalizerAverage') %>%
  mutate(taskType = ifelse(task == 'Jokes', 'Critical', 'Localizer')) 

  #select(-c(contrastName, task)) #%>% #so the spreads/groups work
#spread(taskType, sigChange) %>%
#mutate(sigDiff = Localizer - Critical)

#Okay, so what do we want to know? we want to know if the localizer effect is 
#bigger than the jokes effect in each region.

m1 <- lmer(sigChange ~ taskType*Group + (taskType|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','RHLang','MDRight')))
m0 <- lmer(sigChange ~ taskType+Group + (taskType|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','RHLang','MDRight')))
anova(m1, m0)

#Answer: the interaction matters! But we want more specifics. How about a graph

loctaskstats <- aggregate(localizer2task$sigChange, by=list(localizer2task$Group, 
                                                            localizer2task$taskType, 
                                                            localizer2task$ROIName, 
                                                            localizer2task$ROI), mean)
names(loctaskstats) = c('Group','taskType', 'ROIName', 'ROI','themean')


mybootup = aggregate(localizer2task$sigChange, by=list(localizer2task$Group, 
                                                       localizer2task$taskType, 
                                                       localizer2task$ROIName, 
                                                       localizer2task$ROI), bootup)
names(mybootup) = c('Group','taskType', 'ROIName', 'ROI', 'bootup')
mybootdown = aggregate(localizer2task$sigChange, by=list(localizer2task$Group, 
                                                         localizer2task$taskType, 
                                                         localizer2task$ROIName, 
                                                         localizer2task$ROI), bootdown)
names(mybootdown) = c('Group','taskType', 'ROIName', 'ROI', 'bootdown')

loctaskstats = merge(loctaskstats,mybootup)
loctaskstats = merge(loctaskstats,mybootdown)



ggplot(data=loctaskstats, aes(x=ROIName, y=themean, fill=taskType)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  xlab('') +
  ylab(str_wrap('% signal change, Crit - Control', width=18)) +
  facet_grid(~Group, scale='free_x', space='free_x')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  ggsave(filename="localizer_to_critical_E2.jpg", width=9, height=6) 


#For each region, ask whether there's a difference. There is. This is not a very interesting analysis.  
ToMmodel <- lmer(sigChange ~ taskType + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'ToM'))
ToMmodel0 <- lmer(sigChange ~ 1 + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'ToM'))
anova(ToMmodel, ToMmodel0)  

#Note dropped slopes, didn't converge
MDRmodel <- lmer(sigChange ~ taskType + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group == 'MDRight'))
MDRmodel0 <- lmer(sigChange ~ 1 + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group == 'MDRight'))
anova(MDRmodel, MDRmodel0)  

MDLmodel <- lmer(sigChange ~ taskType + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'MDLeft'))
MDLmodel0 <- lmer(sigChange ~ 1 + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'MDLeft'))
anova(MDLmodel, MDLmodel0)  

RHLmodel <- lmer(sigChange ~ taskType + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'RHLang'))
RHLmodel0 <- lmer(sigChange ~ 1 + (taskType|ROIName) + (taskType|SubjectNumber), data = filter(localizer2task, Group == 'RHLang'))
anova(RHLmodel, RHLmodel0)  

#Note, dropped subject slope for this model, didn't converge
LHLmodel <- lmer(sigChange ~ taskType + (taskType|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group == 'LHLang'))
LHLmodel0 <- lmer(sigChange ~ 1 + (taskType|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group == 'LHLang'))
anova(LHLmodel, LHLmodel0)  


#Ask whether there is a difference between ToM and the other right-hemisphere systems, using same randoms as above
m1 <- lmer(sigChange ~ taskType*Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','MDRight', 'RHLang')))
m0 <- lmer(sigChange ~ taskType+Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','MDRight', 'RHLang')))
anova(m1, m0)

#Followup with ToM over each of the others
m1 <- lmer(sigChange ~ taskType*Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','MDRight')))
m0 <- lmer(sigChange ~ taskType+Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','MDRight')))
anova(m1, m0)

m1 <- lmer(sigChange ~ taskType*Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','RHLang')))
m0 <- lmer(sigChange ~ taskType+Group + (1|ROIName) + (1|SubjectNumber), data = filter(localizer2task, Group %in% c('ToM','RHLang')))
anova(m1, m0)


#EXPLORATORY F
#Each person gets 3 values: in Right Hemisphere, average activation of Lang, MD, ToM. Are people high in one also high in the other two?
systemAvgs <- allSigChange %>%
  filter(contrastName %in% c('joke-lit','H-E','S-N','bel-pho')) %>%
  filter(task == 'Jokes') %>%
  filter(ROI ==0) %>%
  filter(Group %in% c("ToM", "MDRight","RHLang")) %>%
  spread(Group, sigChange) %>%
  gather("OtherSystem", "OthersigChange", c("MDRight", "RHLang")) %>%
  mutate("ROILabel" = ifelse(OtherSystem=="MDRight", "RH Multiple Demand fROIs", "RH Language fROIs"))
  
  
#Let's graph, I'm confused how to compare these. 
ggplot(data=systemAvgs, aes(x=OthersigChange, y=ToM, color=ROILabel)) +
  geom_smooth(method="lm", fill=NA) +
  geom_point() +
  coord_fixed() + 
  scale_x_continuous(breaks=seq(-.1, 2, .1)) +
  scale_y_continuous(breaks=seq(-.1, 2, .1)) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("% signal change on Jokes task in subject-specific fROIs") +
  ylab ("% signal change on Jokes task (Theory of Mind fROIs)") +
  facet_grid(~ROILabel)

#Make a note - single very high outlier on both MD and ToM; make sure test is not contingent on just this subj. 

#Neither is very correlated! Let's quantify that with simple correlation tests. 

cor.test(systemAvgs[systemAvgs$OtherSystem == "MDRight",]$ToM, 
    systemAvgs[systemAvgs$OtherSystem == "MDRight",]$OthersigChange, use='pairwise.complete.obs')

cor.test(systemAvgs[systemAvgs$OtherSystem == "RHLang",]$ToM, 
         systemAvgs[systemAvgs$OtherSystem == "RHLang",]$OthersigChange, use='pairwise.complete.obs')

# #OLD EXPLORATORY F - I MISUNDERSTOOD EV.  - CONFLATED 2 EXPS SHE WANTED
# ########
# #READ IN SPLIT HALF DATA ONLY
# ########
# 
# allSigChange = read.csv(paste(meansig_outputs_folder, 'all_mean_signal_outputs.csv', sep=''))
# 
# allSigChange = allSigChange %>%
#   filter(ind_selection_method == 'Top10Percent') %>%
#   filter(filename %in% c('SplitHalf_RHLfROIs_resp_Jokes_20170904', 
#                                                      'SplitHalf_LangfROIs_resp_Jokes_20170904',
#                                                      'SplitHalf_MDfROIs_resp_Jokes_20170904',
#                                                      'SplitHalf_ToMfROIs_resp_Jokes_20170904')) %>%
#   mutate(task = 'Jokes') %>%  #Oops, the parser messed up these names, fix'em!
#   mutate(fROIs = ifelse(filename == 'SplitHalf_RHLfROIs_resp_Jokes_20170904', 'RHLfROIs', 
#                         ifelse(filename == 'SplitHalf_LangfROIs_resp_Jokes_20170904', 'LangfROIs',
#                                ifelse(filename == 'SplitHalf_MDfROIs_resp_Jokes_20170904', 'MDfROIs', 'ToMfROIs'))))
# 
# # List contrast and ROI names so it's not just numbers!!!!! (This ordering comes from the 
# # standard ordering produced by the 2nd level analyses; we'll arrange differently in the plots)
# 
# RHLangROI.Names = c('RPost Temp', 'RAnt Temp', 'RAngG', 'RIFG',      'RMFG',     'RIFG orb');
# LangROI.Names = c('LPost Temp', 'LAnt Temp', 'LAngG', 'LIFG',      'LMFG',     'LIFG orb');
# 
# MDROI.Names = c('LIFG op',  'RIFG op', 'LMFG',    'RMFG',    'LMFG orb',
#                 'RMFG orb', 'LPrecG', 'RPrecG',  'LInsula', 'RInsula',
#                 'LSMA',    'RSMA',   'LPar Inf', 'RPar Inf', 'LPar Sup',
#                 'RPar Sup', 'LACC',   'RACC');
# 
# ToMROI.Names = c('DM PFC', 'LTPJ',  'MM PFC', 'PC',
#                  'RTPJ',  'VM PFC', 'RSTS');
# 
# splithalf.contrasts = c('ODD_joke-lit', 'EVEN_joke-lit')
# 
# #Split the data into groups by fROIs, and rename them as appropriate
# RHLang_sigs = data.frame(NULL)
# LHLang_sigs = data.frame(NULL)
# MD_sigs = data.frame(NULL)
# ToM_sigs = data.frame(NULL)
# 
# 
# RHLang_sigs = allSigChange %>%
#   filter(fROIs == 'RHLfROIs')%>%
#   mutate(ROIName = RHLangROI.Names[ROI]) %>%
#   group_by(task)%>%
#   mutate(contrastName = splithalf.contrasts[Contrast]) %>%
#   mutate(Group = 'RHLang') %>%
#   ungroup()
# 
# LHLang_sigs = allSigChange %>%
#   filter(fROIs == 'LangfROIs')%>%
#   mutate(ROIName = LangROI.Names[ROI]) %>%
#   group_by(task)%>%
#   mutate(contrastName = splithalf.contrasts[Contrast]) %>%
#   mutate(Group = 'LHLang') %>%
#   ungroup()
# 
# MD_sigs = allSigChange %>%
#   filter(fROIs == 'MDfROIs')%>%
#   mutate(ROIName = MDROI.Names[ROI]) %>%
#   group_by(task)%>%
#   mutate(contrastName = splithalf.contrasts[Contrast]) %>%
#   mutate(Group = ifelse(ROI %%2 == 1, 'MDLeft','MDRight')) %>%
#   ungroup()
# 
# ToM_sigs = allSigChange %>%
#   filter(fROIs == 'ToMfROIs')%>% ##Typo in all the filenames!
#   mutate(ROIName = ToMROI.Names[ROI]) %>%
#   group_by(task)%>%
#   mutate(contrastName = splithalf.contrasts[Contrast]) %>%
#   mutate(Group = 'ToM') %>%
#   ungroup()
# 
# #And stick it all back together!!
# splitHalfSigChange = rbind(RHLang_sigs, LHLang_sigs, MD_sigs, ToM_sigs)
# 
# #(No average - by - network needed for these analyses.)
# 
# ##################
# #Now, to do some split half visualizations and analyses. 
# ##################
# #get the data in shape for some correlations! That means: each observation (human) has 10 * nROI values - odd and even in each fROI in each region
# 
# splitHalfSigChange = splitHalfSigChange %>%
#   select(c(SubjectNumber, Group, ROIName, contrastName, sigChange)) %>%
#   unite(myEntry, Group, ROIName, contrastName, sep=".") %>%
#   spread(myEntry, sigChange) %>%
#   select(-c(SubjectNumber))
# 
# 
# #World's biggest correlation matrix! Each humans' numbers get correlated here. 
# #Whoa! We'll be doing some subsetting...this sure is dumb since I unified the columns above, but oh well. 
# cormat <- cor(splitHalfSigChange, use='pairwise.complete.obs')
# melted_cormat <- melt(cormat) %>%
#   separate(Var1, into = c('Group1','ROIName1','contrastName1'), sep = '\\.') %>%
#   separate(Var2, into = c('Group2','ROIName2','contrastName2'), sep = '\\.')
# 
# #XXXSTARTHERE
# 
# #Rule: only compare across odds and evens, not fROI_12 at ODD and fROI_13 and ODD. 
# #Then, average those two values together. This lets us fairly compare
# #self-to-self correlations with self-to-other correlations
# odd_even <- melted_cormat %>%
#   filter(contrastName1 != contrastName2) %>%
#   select(one_of('Group1','Group2','ROIName1','ROIName2','value')) %>%
#   group_by(Group1, Group2, ROIName1, ROIName2) %>%
#   summarise(meancorr = mean(value))
# 
# #odd_even is the *mean across people* at each individual ROI 
#   
# ggplot(data = odd_even, aes(x=ROIName1, y=ROIName2, fill=meancorr)) + 
#   geom_tile() +
#   facet_wrap(Group1 ~ Group2, scales="free")
# 
# #Cool, that's giant and pretty useless to look at. How to actually represent this in real life? 
# #We want to know whether average within-system correlations are higher than between-system correlations,
# #And also we want to know whether some systems' correlations are higher than others. 
# 
# #So, the nice way to visualize this and then measure it would be:
# #Visualize: Get 1 value per system-pair, i.e. average all the ToMOdd-RHLangEven comparisons and so on
# 
# system_avg = odd_even %>%
#   group_by(Group1, Group2) %>%
#   summarise(meancorr = mean(meancorr))
# 
# #We'd like to put hemispheres of systems together here, since they are actually correlated!
# system_avg$Group1 <-factor(system_avg$Group1, levels = c('LHLang','MDLeft','RHLang','MDRight','ToM'))
# system_avg$Group2 <-factor(system_avg$Group2, levels = c('LHLang','MDLeft','RHLang','MDRight','ToM'))
# 
# ggplot(data = system_avg, aes(x=Group1, y=Group2, fill=meancorr)) + 
#   geom_tile()
# 
# #Measure: We have a set of system-pair vectors. Compare them, probably not with parametric tests? I'm
# #not sure, we'll plot them to check. 
# 
#   
#   
