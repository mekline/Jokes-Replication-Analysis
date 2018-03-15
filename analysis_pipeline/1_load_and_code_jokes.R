# Start here to reproduce the main (confirmatory) analyses for the Jokes paper
#This file reads in ALL the %-signal-change values, per-participant, per-parcel, per-contrast,
# Those %-signal-change calculations are produced by the awesome toolbox analyses, and represent a single overall calculation
#derived for the whole parcel region (not individual voxels, as mk sometimes forgets)

#ALL packages necessary for the analysis pipeline should get loaded here
rm(list = ls())
library(bootstrap)
library(dplyr)
library(ggplot2)
library(lme4)
library(pwr)
library(stringr)
library(tidyr)

#Set your working directory here
repodir = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/"
analysis_folder = paste(repodir, 'analysis_pipeline/', sep='')
meansig_outputs_folder = paste(repodir, 'E2_meansignal_outputs/', sep='')
setwd(analysis_folder)


########
#READ IN DATA - OR SKIP TO FILE XXXXXX TO PROCEED FROM CLEANED AND LABELED DATA
########

allSigChange = read.csv(paste(meansig_outputs_folder, 'all_mean_signal_outputs.csv', sep=''))

#FOR NOW: Make a choice whether to do all analyses with top 50 voxels or top 10% voxels 
#(This can be changed to 'Top50Voxels' to see all results with that fROI selection procedure
allSigChange = filter(allSigChange, ind_selection_method == 'Top10Percent')

#NEW: Also, screen out the split-half analyses for now; we'll treat those in their own file since they are exploratory
allSigChange = filter(allSigChange, !filename %in% c('SplitHalf_RHLfROIs_resp_Jokes_20170904', 
                                                     'SplitHalf_LangfROIs_resp_Jokes_20170904',
                                                     'SplitHalf_MDfROIs_resp_Jokes_20170904',
                                                     'SplitHalf_ToMfROIs_resp_Jokes_20170904'))


#CHECKSUM - The number of rows in allSigChange should be the same 
#after the below naming procedure (see line 136)
nrow(allSigChange) #8489


# List contrast and ROI names so it's not just numbers!!!!! (This ordering comes from the 
# standard ordering produced by the 2nd level analyses; we'll arrange differently in the plots)

RHLangROI.Names = c('RPost Temp', 'RAnt Temp', 'RAngG', 'RIFG',      'RMFG',     'RIFG orb');
LangROI.Names = c('LPost Temp', 'LAnt Temp', 'LAngG', 'LIFG',      'LMFG',     'LIFG orb');

MDROI.Names = c('LIFG op',  'RIFG op', 'LMFG',    'RMFG',    'LMFG orb',
                      'RMFG orb', 'LPrecG', 'RPrecG',  'LInsula', 'RInsula',
                      'LSMA',    'RSMA',   'LPar Inf', 'RPar Inf', 'LPar Sup',
                      'RPar Sup', 'LACC',   'RACC');

ToMROI.Names = c('DM PFC', 'LTPJ',  'MM PFC', 'PC',
                      'RTPJ',  'VM PFC', 'RSTS');

joke.contrasts = c('joke', 'lit', 'joke-lit')
jokecustom.contrasts = c('low','med','high', 'linear') #Bug solved! I didn't record 'other' (no response) in the toolbox output this time. NBD. 
jokecustom.forcloudy.contrasts = c('low','med','high','other','paramfun') #Apparently used different cond names for these ones.
lang.contrasts = c('S','N','S-N')
MD.contrasts = c('H','E','H-E')
ToM.contrasts = c('bel','pho','bel-pho')
Cloudy.contrasts = c('ment','pain','phys','reln')

#Small fix to make the splitting work right - standardize capitalization!
allSigChange <- allSigChange %>%
  mutate(correctedFROIs = as.factor(str_c(str_sub(fROIs, 1, -2), 's')))%>%
  mutate(fROIs = correctedFROIs)%>% #(dropping the old fROIs column)
  select(-correctedFROIs)

#Split the data into groups by fROI sets, and rename them as appropriate (messy, but done separately to deal with various
#inconsistencies in the data organization)
#(Within each, we check which task applies, and add the crit-contrasts that way)
RHLang_sigs = data.frame(NULL)
LHLang_sigs = data.frame(NULL)
MD_sigs = data.frame(NULL)
ToM_sigs = data.frame(NULL)
CloudyToM_sigs = data.frame(NULL)


RHLang_sigs = allSigChange %>%
  filter(fROIs == 'RHLfROIs')%>%
  mutate(ROIName = RHLangROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = ifelse(task == 'Jokes', joke.contrasts[Contrast], 
                               ifelse(task == 'JokesCustom', jokecustom.contrasts[Contrast], 
                               lang.contrasts[Contrast]))) %>%
  mutate(ROIMask = 'RHLang') %>%
  mutate(localizer = 'Lang') %>%
  ungroup()

LHLang_sigs = allSigChange %>%
  filter(fROIs == 'LangfROIs')%>%
  mutate(ROIName = LangROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = ifelse(task == 'Jokes', joke.contrasts[Contrast], 
                               ifelse(task == 'JokesCustom', jokecustom.contrasts[Contrast], 
                               lang.contrasts[Contrast]))) %>%
  mutate(ROIMask = 'LHLang') %>%
  mutate(localizer = 'Lang') %>%
  ungroup()

MD_sigs = allSigChange %>%
  filter(fROIs == 'MDfROIs')%>%
  mutate(ROIName = MDROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = ifelse(task == 'Jokes', joke.contrasts[Contrast], 
                               ifelse(task == 'JokesCustom', jokecustom.contrasts[Contrast], 
                               MD.contrasts[Contrast]))) %>%
  mutate(ROIMask = ifelse(ROI %%2 == 1, 'MDLeft','MDRight')) %>%
  mutate(localizer = 'MD') %>%
  ungroup()

ToM_sigs = allSigChange %>%
  filter(fROIs == 'ToMfROIs')%>%
  mutate(ROIName = ToMROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = ifelse(task == 'Jokes', joke.contrasts[Contrast], 
                               ifelse(task == 'JokesCustom', jokecustom.contrasts[Contrast], 
                               ifelse(task == 'Cloudy', Cloudy.contrasts[Contrast],
                               ToM.contrasts[Contrast])))) %>%
  mutate(ROIMask = 'ToM') %>%
  mutate(localizer = 'ToM') %>%
  ungroup()

CloudyToM_sigs = allSigChange %>%
  filter(fROIs == 'CloudyToMfROIs')%>%
  mutate(ROIName = ToMROI.Names[ROI]) %>%
  group_by(task) %>%
  mutate(contrastName = ifelse(task == 'Jokes', joke.contrasts[Contrast], 
                               ifelse(task == 'JokesCustom', jokecustom.forcloudy.contrasts[Contrast], 
                               Cloudy.contrasts[Contrast]))) %>%
  mutate(ROIMask = 'ToM') %>%
  mutate(localizer = 'Cloudy') %>%
  ungroup()


#And stick it all back together!!
allSigChange = rbind(RHLang_sigs, LHLang_sigs, MD_sigs, ToM_sigs, CloudyToM_sigs)

#CHECKSUM - make sure no analyses were lost and that name matching went correctly!
nrow(allSigChange) #Should be 8489
aggregate(allSigChange$average.localizer.mask.size, by=list(allSigChange$ROIName, allSigChange$ROI.size), mean) #Checks that ROINames were applied correctly!

#Arrange and factorize columns
#Pick allSigChange columns to retain (removing columns with arbitrary numbering systems. Names are better!)
allSigChange <- allSigChange %>%
  select(-one_of("ROI", "ROI.size","average.localizer.mask.size","inter.subject.overlap","Contrast", "fROIs"))

allSigChange$SubjectNumber <- as.factor(allSigChange$SubjectNumber)
allSigChange$task <- as.factor(allSigChange$task)
allSigChange$localizer <- as.factor(allSigChange$localizer)
allSigChange$ROIMask <- as.factor(allSigChange$ROIMask)
allSigChange$ROIName <- as.factor(allSigChange$ROIName)
allSigChange$contrastName <- as.factor(allSigChange$contrastName)

#Add average signal change by ROIMask
#In addition to the by-region signal changes, we are going to give each person an average signal change value for each localizer, each task
avgSigChange = aggregate(allSigChange$sigChange, by=list(allSigChange$ROIMask, allSigChange$localizer, allSigChange$task, allSigChange$SubjectNumber,allSigChange$contrastName), mean)
names(avgSigChange) = c('ROIMask', 'localizer', 'task', 'SubjectNumber', 'contrastName','sigChange')
avgSigChange <- avgSigChange %>%
  mutate(ROIName = 'LocalizerAverage')

avgSigChange <- allSigChange %>%
  group_by(ROIMask, localizer, task, contrastName, SubjectNumber)%>%
  summarize(sigChange = mean(sigChange), 
            filename = first(filename), 
            ind_selection_method=first(ind_selection_method),
            pipeline = first(pipeline),
            ROIName = 'LocalizerAverage')


allSigChange <- bind_rows(allSigChange, avgSigChange)

#New plan 1/18/18 - save this df to avoid messy rerunning. 
setwd(meansig_outputs_folder)
save(allSigChange, file = "allSigChange.RData")
setwd(analysis_folder)
save(allSigChange, file = "allSigChange.RData")

#########
# Get summary stats for the Jokes & JokesCustom tasks (used for graphing & reporting effect size measures)
#########

#Drop the contrasts (localizer results) we're not interested in for graphing...
toGraph = allSigChange %>%
  filter(contrastName %in% c('joke','lit','high','med','low'))

#Next, get the table that we'll be making the graphs from: for each region (including the average region), take all 
#the individual signal changes and calculate a mean and a standard error
sterr <- function(mylist){
  my_se = sd(mylist)/sqrt(length(mylist)) 
  
  return(my_se)
}

mystats = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI,toGraph$contrastName), mean)
names(mystats) = c('Group','Task', 'ROIName', 'ROI','contrastName', 'themean')
myster = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI,toGraph$contrastName), sterr)
names(myster) = c('Group','Task', 'ROIName', 'ROI','contrastName', 'sterr')

mystats = merge(mystats,myster)
mystats$se_up = mystats$themean + mystats$sterr
mystats$se_down = mystats$themean - mystats$sterr

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
mybootup = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI, toGraph$contrastName), bootup)
names(mybootup) = c('Group', 'Task', 'ROIName', 'ROI','contrastName', 'bootup')
mybootdown = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI, toGraph$contrastName), bootdown)
names(mybootdown) = c('Group', 'Task', 'ROIName', 'ROI','contrastName', 'bootdown')

mystats = merge(mystats,mybootup)
mystats = merge(mystats,mybootdown)

#########
# Effect size reports
#########
#For the main analysis in the paper (signal change jokes>nonjokes) we'll report  a simple measure of effect size: the
#mean signal change in each system. Here they are:
eff <- mystats %>%
  filter(ROIName == "LocalizerAverage") %>%
  filter(contrastName == 'joke' | contrastName == 'lit') %>%
  select(c("Group", "contrastName", "themean")) %>%
  spread(contrastName, themean) %>%
  mutate(sigChange = joke-lit)
  
            
#########
# Graphs!
#########

#Now we can use the information stored in mystats to make pretty graphs! This could be done in excel too by printing mystats
#Change to figs output folder
setwd("./figs")


#Select the rows we want for each graph, and order them how we want! For now, localizerAverage will just come first in all sets
mystats$contNo <- 1
mystats[mystats$contrastName == 'joke',]$contNo <- 1
mystats[mystats$contrastName == 'lit',]$contNo <- 2
mystats[mystats$contrastName == 'high',]$contNo <- 1
mystats[mystats$contrastName == 'med',]$contNo <- 2
mystats[mystats$contrastName == 'low',]$contNo <- 3
#mystats = arrange(mystats, ROI)
mystats = arrange(mystats, contNo)

#Add a new col grouping to separate out the localizer average
mystats$ROIGroup <- ""
mystats[mystats$ROIName == "LocalizerAverage",]$ROIGroup <- "across fROIs"
mystats = arrange(mystats, desc(ROIGroup))

#Changes for prettiness
mystats[mystats$ROIName=="LocalizerAverage",]$ROIName <- "average across fROIs"
mystats$ROIName <- str_wrap(mystats$ROIName, width = 4)

mystats$contrastLabel <- mystats$contrastName
mystats[mystats$contrastName == "joke",]$contrastLabel <- "Jokes\n  "
mystats[mystats$contrastName == "lit",]$contrastLabel <- "Non-jokes\n   "
mystats[mystats$contrastName == "high",]$contrastLabel <- "high\n  "
mystats[mystats$contrastName == "med",]$contrastLabel <- "med\n   "
mystats[mystats$contrastName == "low",]$contrastLabel <- "low\n  "



#XXX START HERE TO SPLIT INTO REG AND CUSTOM (exploratory!) ASSIGNMENTS!!!

#Subsets & Ordering (elaborate code, probably can condense these; ggplot is finicky at orders)
RHLang = filter(mystats, Group == 'RHLang', Task == 'Jokes')
RHLang <- RHLang[order(RHLang$ROI),]
RHLang$PresOrder = c(13,14, 9,10, 7,8, 11,12, 3,4,5,6,1,2) #Reorder for standard presentation!
RHLang <- RHLang[order(RHLang$PresOrder),]
RHLang = arrange(RHLang, desc(ROIGroup))

RHLangCustom = filter(mystats, Group == 'RHLang', Task == 'JokesCustom')
RHLangCustom <- RHLangCustom[order(RHLangCustom$ROI),]
RHLangCustom$PresOrder = c(19,20,21, 13,14,15, 10,11,12, 16,17,18, 4,5,6, 7,8,9, 1,2,3) #Reorder for standard presentation!
RHLangCustom <- RHLangCustom[order(RHLangCustom$PresOrder),]
RHLangCustom = arrange(RHLangCustom, desc(ROIGroup))


LHLang = filter(mystats, Group == 'LHLang', Task == 'Jokes')
LHLang <- LHLang[order(LHLang$ROI),]
LHLang$PresOrder = c(13,14, 9,10, 7,8, 11,12, 3,4,5,6,1,2)
LHLang <- LHLang[order(LHLang$PresOrder),]
LHLang = arrange(LHLang, desc(ROIGroup))

LHLangCustom = filter(mystats, Group == 'LHLang', Task == 'JokesCustom')
LHLangCustom <- LHLangCustom[order(LHLangCustom$ROI),]
LHLangCustom$PresOrder = c(19,20,21, 13,14,15, 10,11,12, 16,17,18, 4,5,6, 7,8,9, 1,2,3) 
LHLangCustom <- LHLangCustom[order(LHLangCustom$PresOrder),]
LHLangCustom = arrange(LHLangCustom, desc(ROIGroup))


MDLeft = filter(mystats, Group == 'MDLeft', Task == 'Jokes')
MDLeft <- MDLeft[order(MDLeft$ROI),]
MDLeft = arrange(MDLeft, desc(ROIGroup))

MDLeftCustom = filter(mystats, Group == 'MDLeft', Task == 'JokesCustom')
MDLeftCustom <- MDLeftCustom[order(MDLeftCustom$ROI),]
MDLeftCustom = arrange(MDLeftCustom, desc(ROIGroup))

MDRight = filter(mystats, Group == 'MDRight', Task == 'Jokes')
MDRight <- MDRight[order(MDRight$ROI),]
MDRight = arrange(MDRight, desc(ROIGroup))

MDRightCustom = filter(mystats, Group == 'MDRight', Task == 'JokesCustom')
MDRightCustom <- MDRightCustom[order(MDRightCustom$ROI),]
MDRightCustom = arrange(MDRightCustom, desc(ROIGroup))

ToM = filter(mystats, Group == 'ToM', Task == 'Jokes')
ToM <- ToM[order(ToM$ROI),]
# ToM$PresOrder = c(1,2,3,4,9,10,5,6,7,8,11,12,13,14) This is for when VMPFC is NOT included
ToM$PresOrder = c(1,2,3,4,9,10,5,6,7,8,11,12,13,14,15,16) #This is for all contrasts
ToM <- ToM[order(ToM$PresOrder),]
ToM = arrange(ToM, desc(ROIGroup))

ToMCustom = filter(mystats, Group == 'ToM', Task == 'JokesCustom')
ToMCustom <- arrange(ToMCustom, contNo)
ToMCustom <- ToMCustom[order(ToMCustom$ROI),]
ToMCustom$PresOrder = c(1,2,3,4,5,6,13,14,15,7,8,9,10,11,12,16,17,18,19,20,21, 22, 23, 24)
ToMCustom <- ToMCustom[order(ToMCustom$PresOrder),]
ToMCustom = arrange(ToMCustom, desc(ROIGroup))

Cloudy = filter(mystats, Group == 'ToM_by_Cloudy', Task == 'Jokes')
Cloudy <- Cloudy[order(Cloudy$ROI),]
Cloudy$PresOrder = c(1,2,3,4,9,10,5,6,7,8,11,12,13,14,15,16) #This is for all contrasts
Cloudy <- Cloudy[order(Cloudy$PresOrder),]
Cloudy = arrange(Cloudy, desc(ROIGroup))
#Graphing function!

makeBar = function(plotData,ylow=-0.5,yhigh=2.5, mycolors = c("gray35", "gray60")) {

  #freeze factor orders
  plotData$ROIName <- factor(plotData$ROIName, levels = unique(plotData$ROIName))
  plotData$ROIGroup <- factor(plotData$ROIGroup, levels = unique(plotData$ROIGroup))
  plotData$contrastLabel <- factor(plotData$contrastLabel, levels = unique(plotData$contrastLabel))
  myfi = paste(plotData$Group[1], '_', plotData$Task[2], '.jpg', sep="")#filename
  print(myfi)

ggplot(data=plotData, aes(x=ROIName, y=themean, fill=contrastLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(ylow,yhigh)) +
  scale_y_continuous(breaks = seq(-0.5, 2.5, 0.5))+
  xlab('') +
  ylab(str_wrap('% signal change over fixation', width=18)) +
  scale_fill_manual(name="", values=mycolors) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(text = element_text(size = 40)) +
  facet_grid(~ROIGroup, scale='free_x', space='free_x') +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_blank()) 
  # Optional, remove for RHLang and ToMCustom since we want the legend there...
  #+ theme(legend.position="none")
 

  ggsave(filename=myfi, width=length(unique(plotData$ROIName))*2.2, height=6.1)
  
}

makeBar(LHLang)
makeBar(RHLang)
makeBar(MDLeft)
makeBar(MDRight)
makeBar(ToM, -0.5, 1)
makeBar(Cloudy, -0.5, 1)

makeBar(ToMCustom, -0.5, 1, c("high\n  "= "gray35", "med\n   "= "gray50", "low\n  "= "gray65"))
makeBar(RHLangCustom, mycolors = c("high\n  "= "gray35", "med\n   "= "gray50", "low\n  "= "gray65"))
makeBar(LHLangCustom, mycolors = c("high\n  "= "gray35", "med\n   "= "gray50", "low\n  "= "gray65"))
makeBar(MDLeftCustom, mycolors = c("high\n  "= "gray35", "med\n   "= "gray50", "low\n  "= "gray65"))
makeBar(MDRightCustom, mycolors = c("high\n  "= "gray35", "med\n   "= "gray50", "low\n  "= "gray65"))
