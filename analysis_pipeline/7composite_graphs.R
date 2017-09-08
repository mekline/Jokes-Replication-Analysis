#Composite graphs! Warning, this assumes you've run the Exp 2 pipeline (especially 1, 2, and 5 to load and
#format the data.  Script begins by testing this, and making sure you're in the right directory still)

#(set your own wd first)
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
mywd = "/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline"
View(avgRT)
View(avgResponse)
View(allSigChange)

#STOP HERE and look if avgRT for this data (Exp 2) is already in milliseconds. If not, make it so!
avgRT$meanRT
#avgRT$meanRT <- avgRT$meanRT * 1000

##############
#Now load up the data saved from experiment 1, and adjust column names to match
##############

setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/E1_tabular_data")

avgRT_E1 = read.csv('avgRT_Behavioral_Exp1.csv')
avgResponse_E1 = read.csv('avgResponse_Behavioral_Exp1.csv')
allSigChange_E1 = read.csv('allSigChange_Exp1.csv')

allSigChange_E1 = allSigChange_E1 %>%
  mutate(task = ifelse((contrastName == 'joke') | (contrastName == 'lit')| (contrastName == 'joke-lit'), 
                       'Jokes', 'JokesCustom')) %>%
  mutate(Experiment = 'Experiment 1')

allSigChange = allSigChange %>%
  select(-one_of(c('participants','ID'))) %>%
  mutate(Experiment = 'Experiment 2')

avgRT_E1 = avgRT_E1 %>%
  mutate(ID = newSubjectName) %>%
  mutate(Experiment = 'Experiment 1') %>%
  select(-newSubjectName)

avgResponse_E1 = avgResponse_E1 %>%
  mutate(ID = newSubjectName) %>%
  mutate(Experiment = 'Experiment 1') %>%
  select(-newSubjectName)

avgRT = mutate(avgRT, Experiment = 'Experiment 2')
avgResponse <- mutate(avgResponse, Experiment = 'Experiment 2')

#And merge the datasets
avgRT_E1$category <- as.factor(avgRT_E1$category)
avgResponse_E1$category <- as.factor(avgResponse_E1$category)
avgRT_E1$ID <- as.factor(avgRT_E1$ID)
avgResponse_E1$ID <- as.factor(avgResponse_E1$ID)

avgRT_E1$Experiment <- as.factor(avgRT_E1$Experiment)
avgResponse_E1$Experiment <- as.factor(avgResponse_E1$Experiment)

avgRT_E1$meanRT <- as.numeric(as.character(avgRT_E1$meanRT))
avgResponse_E1$meanResponse <- as.numeric(as.character(avgResponse_E1$meanResponse))

#And merge at last!
all_allSignalChange = merge(allSigChange, allSigChange_E1, all.x = TRUE, all.y = TRUE)
all_avgRT = merge(avgRT, avgRT_E1, all.x = TRUE, all.y = TRUE)
all_avgResponse = merge(avgResponse, avgResponse_E1, all.x = TRUE, all.y = TRUE)

#Whoops! RTs should be in milliseconds. Experiment 1 probably isn't yet. 
all_avgRT[all_avgRT$Experiment == 'Experiment 1',]$meanRT <- all_avgRT[all_avgRT$Experiment == 'Experiment 1',]$meanRT * 1000

##############
#Fig 2 - behavioral, RT and funny-ness ratings
#(Borrow code from the single-study behavioral stuffs.)
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
all_avgRT <- ungroup(all_avgRT)
all_avgResponse <- ungroup(all_avgResponse)

#rename categories
all_avgRT$categoryLabel <- ""
all_avgRT[all_avgRT$category == "joke",]$categoryLabel <- "Jokes"
all_avgRT[all_avgRT$category == "nonjoke",]$categoryLabel <- "Non-Jokes"
all_avgResponse$categoryLabel <- ""
all_avgResponse[all_avgResponse$category == "joke",]$categoryLabel <- "Jokes"
all_avgResponse[all_avgResponse$category == "nonjoke",]$categoryLabel <- "Non-Jokes"

toPlotRT = all_avgRT %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(mean = mean(meanRT))

tobootUp = all_avgRT %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(bootup = bootup(meanRT))
tobootDown = all_avgRT %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(bootdown = bootdown(meanRT))

toPlotRT = merge(toPlotRT, tobootUp)
toPlotRT = merge(toPlotRT, tobootDown)


toPlotResp = all_avgResponse %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(mean = mean(meanResponse))


tobootUp = all_avgResponse %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(bootup = bootup(meanResponse))
tobootDown = all_avgResponse %>%
  group_by(categoryLabel, Experiment)%>%
  summarise(bootdown = bootdown(meanResponse))

toPlotResp = merge(toPlotResp, tobootUp)
toPlotResp = merge(toPlotResp, tobootDown)

setwd(paste(getwd(), '/analysis_pipeline/figs', sep=''))

############ Graph - Reaction time
ggplot(data=toPlotRT, aes(y=mean, x=categoryLabel, fill = Experiment)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,1200)) +
  scale_y_continuous(breaks = seq(0, 2000, 200))+
  xlab('') +
  ylab('Response time (milliseconds)') +
  scale_fill_manual(name="", values=c("gray35", "gray60")) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none")  
ggsave(filename="composite_E1_rep_behavioralrt.jpg", width=2, height=3)


############Graph - Reponse (funniness)
ggplot(data=toPlotResp, aes(y=mean, x=categoryLabel, fill = Experiment)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(1,4)) +
  scale_y_continuous(breaks = seq(1, 4, 1))+
  xlab('') +
  ylab('Average funny-ness rating') +
  scale_fill_manual(name="", values=c("gray35", "gray60")) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(strip.background = element_blank()) +
  
ggsave(filename="composite_E1_rep_behavioral.jpg", width=3, height=3)


##############
##############
#Fig 3 - Signal change (system averages only!) in each system, by experiment
##############

toGraph <- all_allSignalChange %>%
  filter(contrastName %in% c('joke','lit', 'high', 'med','low', 'joke-lit')) %>%
  mutate(newSubjName = paste(Experiment, SubjectNumber))

sterr <- function(mylist){
  my_se = sd(mylist)/sqrt(length(mylist)) 
  
  return(my_se)
}

mystats = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI,toGraph$contrastName, toGraph$Experiment), mean)
names(mystats) = c('Group','Task', 'ROIName', 'ROI','contrastName', 'Experiment', 'themean')
myster = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI,toGraph$contrastName, toGraph$Experiment), sterr)
names(myster) = c('Group','Task', 'ROIName', 'ROI','contrastName', 'Experiment','sterr')

mystats = merge(mystats,myster)
mystats$se_up = mystats$themean + mystats$sterr
mystats$se_down = mystats$themean - mystats$sterr

#Edit! We should be doing bootstrapped 95% confidence intervals instead! calculate them from allSigChange
#then merge into mystats

mybootup = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI, toGraph$contrastName, toGraph$Experiment), bootup)
names(mybootup) = c('Group', 'Task', 'ROIName', 'ROI','contrastName', 'Experiment', 'bootup')
mybootdown = aggregate(toGraph$sigChange, by=list(toGraph$Group, toGraph$task, toGraph$ROIName, toGraph$ROI, toGraph$contrastName, toGraph$Experiment), bootdown)
names(mybootdown) = c('Group', 'Task', 'ROIName', 'ROI','contrastName', 'Experiment', 'bootdown')

mystats = merge(mystats,mybootup)
mystats = merge(mystats,mybootdown)


#Select the rows we want for each graph, and order them how we want! For now, localizerAverage will just come first in all sets
mystats$contNo <- 1
mystats[mystats$contrastName == 'joke',]$contNo <- 1
mystats[mystats$contrastName == 'lit',]$contNo <- 2
mystats = arrange(mystats, contNo)

#Changes for prettiness
mystats[mystats$ROIName=="LocalizerAverage",]$ROIName <- "average across fROIs"
mystats$ROIName <- str_wrap(mystats$ROIName, width = 4)

mystats$contrastLabel <- mystats$contrastName
mystats[mystats$contrastName == "joke",]$contrastLabel <- "Jokes\n  "
mystats[mystats$contrastName == "lit",]$contrastLabel <- "Non-Jokes\n   "

mystats$Group <- factor(mystats$Group, levels = c("ToM", "RHLang", "MDRight", "LHLang", "MDLeft", "ToMCustom"))
mystats <- mutate(mystats, GroupLabel = ifelse(Group == "ToM", "Theory of mind network,\nRight hemisphere",
                                               ifelse(Group == "RHLang", "Language network, \nRight hemisphere",
                                                      ifelse(Group == "MDRight", "Multiple demand network,\nRight hemisphere",
                                                             ifelse(Group == "LHLang", "Language network,\nLeft hemisphere",
                                                                    ifelse(Group == "MDLeft", "Multiple demand network,\nLeft hemisphere","ToMCustom")
                                                                    )))))

#ARE YOU KIDDING ME, R.  More factor order setting. 
mystats$GroupLabel <- factor(mystats$GroupLabel, levels=c("Theory of mind network,\nRight hemisphere", 
                                                          "Language network, \nRight hemisphere",
                                                          "Multiple demand network,\nRight hemisphere",
                                                          "Language network,\nLeft hemisphere",
                                                          "Multiple demand network,\nLeft hemisphere"
                                                          ))


#Graphing function!

makeBar = function(plotData,ylow=-0.4,yhigh=3, mycolors = c("gray35", "gray60")) {
  
  #freeze factor orders
  plotData$ROIName <- factor(plotData$ROIName, levels = unique(plotData$ROIName))
  plotData$contrastLabel <- factor(plotData$contrastLabel, levels = unique(plotData$contrastLabel))
  myfi = paste('composite_AllRegions_', plotData$Task[2], '.jpg', sep="")#filename
  print(myfi)
  
  ggplot(data=plotData, aes(x=contrastLabel, y=themean, fill=Experiment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
    coord_cartesian(ylim=c(ylow,yhigh)) +
    scale_y_continuous(breaks = seq(0, 1, 0.5))+
    xlab('') +
    ylab(str_wrap('% signal change over fixation', width=18)) +
    scale_fill_manual(name="", values=mycolors) +
    facet_grid(~GroupLabel) +
    theme_bw() +
    theme(legend.key = element_blank()) +
    theme(legend.position = 'bottom') +
    #theme(strip.background = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank())
  # Optional, remove for RHLang and ToMCustom since we want the legend there...
  #+ theme(legend.position="none")
  
  
  ggsave(filename=myfi, width=10, height=4)
  
}

#Localizer averages only, regular condition assignment
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline/figs")
mylocs = mystats %>%
  filter(ROIName == 'average\nacross\nfROIs') %>%
  filter(contrastName %in% c('joke','lit'))
makeBar(mylocs)

#OKAY. Also, Ev would like a line graph below, showing how *signal change* Jokes > Nonjokes in each system
#compares across experiments. Let's do it. 


makeLine = function(plotData,ylow=-0.1,yhigh=0.6, mycolors = c("gray35", "gray60")) {
  
  #freeze factor orders
  plotData$ROIName <- factor(plotData$ROIName, levels = unique(plotData$ROIName))
  plotData$contrastLabel <- factor(plotData$contrastLabel, levels = unique(plotData$contrastLabel))
  myfi = paste('composite_EffectSizeLine_', plotData$Task[2], '.jpg', sep="")#filename
  print(myfi)
  
  ggplot(data=plotData, aes(x=contrastLabel, y=themean, color=Experiment)) + 
    geom_errorbar(aes(ymin=bootdown, ymax=bootup, colour=factor(Experiment)), width=.05, position=position_dodge(.15)) +
    geom_point(stat="identity", position=position_dodge(.15)) +
    coord_cartesian(ylim=c(ylow,yhigh)) +
    scale_y_continuous(breaks = seq(0, 0.5, 0.25))+
    xlab('') +
    ylab(str_wrap('% signal change Joke > Non-Joke', width=18)) +
    scale_color_manual(name="", values=mycolors) +
    facet_grid(~GroupLabel) +
    theme_bw() +
    theme(legend.key = element_blank()) +
    theme(strip.background = element_blank()) +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  # Optional, remove for RHLang and ToMCustom since we want the legend there...
  #+ theme(legend.position="none")
  
  
  ggsave(filename=myfi, width=9, height=2)
  
}

#Localizer averages only, regular condition assignment (will add lines between manually)
mylocs = mystats %>%
  filter(ROIName == 'average\nacross\nfROIs') %>%
  filter(contrastName %in% c('joke-lit'))
makeLine(mylocs)
########################
#Figure 4 - TOM with custom by-person assignments
########################
########################

mystats[mystats$Group == 'ToMCustom',]$Group <- "ToM"

ToMCustom = filter(mystats, Group == 'ToM', Task == 'JokesCustom')
ToMCustom <- arrange(ToMCustom, contNo)
ToMCustom <- ToMCustom[order(ToMCustom$ROI),]
ToMCustom$PresOrder = c(1,2,3,4,5,6,13,14,15,7,8,9,10,11,12,16,17,18,19,20,21, 22, 23, 24)
ToMCustom <- ToMCustom[order(ToMCustom$PresOrder),]

makeRegionsBar = function(plotData,ylow=-0.5,yhigh=2.25, mycolors = c("gray35", "gray60")) {
  
  #freeze factor orders
  plotData$ROIName <- factor(plotData$ROIName, levels = unique(plotData$ROIName))
  nROI = length(unique(plotData$ROIName))
  plotData$contrastLabel <- factor(plotData$contrastLabel, levels = unique(plotData$contrastLabel)[c(1,3,2)])
  myfi = paste('composite_', plotData$Group[2], plotData$Task[2], '.jpg', sep="")#filename
  print(myfi)
  
  ggplot(data=plotData, aes(x=contrastLabel, y=themean, fill=Experiment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=bootdown, ymax=bootup), colour="black", width=.1, position=position_dodge(.9)) +
    coord_cartesian(ylim=c(ylow,yhigh)) +
    scale_y_continuous(breaks = seq(-0.5, 2, 0.5))+
    xlab('') +
    ylab(str_wrap('% signal change over fixation', width=18)) +
    scale_fill_manual(name="", values=mycolors) +
    facet_grid(~ROIName, scale='free_x', space='free_x')+
    theme_bw() +
    theme(legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position="none")

  ggsave(filename=myfi, width=nROI+1, height=3)
  
}

makeRegionsBar(ToMCustom)

#####################################################
#
# Figures for the Supplemental - parcel by parcel graphs, for each system on the Jokes > Non-Jokes contrast
# This uses the same makeRegionsBar function, just need to organize and prettify each system here; stealing code from the original figs script
#####################################################

mystats <- mystats %>%
  mutate(ROIGroup = ifelse((ROIName == "average\nacross\nfROIs"), 1, 2))

#Subsets & Ordering (elaborate code, probably can condense these; ggplot is finicky at orders)
RHLang = filter(mystats, Group == 'RHLang', Task == 'Jokes', (contrastName == 'joke' | contrastName == 'lit'))
RHLang <- RHLang[order(RHLang$ROI),]
RHLang$PresOrder = c(13,14, 9,10, 7,8, 11,12, 3,4,5,6,1,2) #Reorder for standard presentation!
RHLang <- RHLang[order(RHLang$PresOrder),]
RHLang = arrange(RHLang, ROIGroup)
makeRegionsBar(RHLang)

LHLang = filter(mystats, Group == 'LHLang', Task == 'Jokes', (contrastName == 'joke' | contrastName == 'lit'))
LHLang <- LHLang[order(LHLang$ROI),]
LHLang$PresOrder = c(13,14, 9,10, 7,8, 11,12, 3,4,5,6,1,2)
LHLang <- LHLang[order(LHLang$PresOrder),]
LHLang = arrange(LHLang, ROIGroup)
makeRegionsBar(LHLang)

MDLeft = filter(mystats, Group == 'MDLeft', Task == 'Jokes', (contrastName == 'joke' | contrastName == 'lit'))
MDLeft <- MDLeft[order(MDLeft$ROI),]
MDLeft = arrange(MDLeft, ROIGroup)
makeRegionsBar(MDLeft)

MDRight = filter(mystats, Group == 'MDRight', Task == 'Jokes', (contrastName == 'joke' | contrastName == 'lit'))
MDRight <- MDRight[order(MDRight$ROI),]
MDRight = arrange(MDRight, ROIGroup)
makeRegionsBar(MDRight)

ToM = filter(mystats, Group == 'ToM', Task == 'Jokes', (contrastName == 'joke' | contrastName == 'lit'))
ToM <- ToM[order(ToM$ROI),]
# ToM$PresOrder = c(1,2,3,4,9,10,5,6,7,8,11,12,13,14) This is for when VMPFC is NOT included
ToM$PresOrder = c(1,2,3,4,9,10,5,6,7,8,11,12,13,14,15,16) #This is for all contrasts
ToM <- ToM[order(ToM$PresOrder),]
ToM = arrange(ToM, ROIGroup)
makeRegionsBar(ToM)

#NOTE remember I've only done 1st levels on Custom for the non-TOM regions for Exp 2
RHLangCustom = filter(mystats, Group == 'RHLang', Task == 'JokesCustom')
RHLangCustom <- RHLangCustom[order(RHLangCustom$ROI),]
RHLangCustom$PresOrder = c(19,20,21, 13,14,15, 10,11,12, 16,17,18, 4,5,6, 7,8,9, 1,2,3) #Reorder for standard presentation!
RHLangCustom <- RHLangCustom[order(RHLangCustom$PresOrder),]
RHLangCustom = arrange(RHLangCustom, ROIGroup)
makeRegionsBar(RHLangCustom)

LHLangCustom = filter(mystats, Group == 'LHLang', Task == 'JokesCustom')
LHLangCustom <- LHLangCustom[order(LHLangCustom$ROI),]
LHLangCustom$PresOrder = c(19,20,21, 13,14,15, 10,11,12, 16,17,18, 4,5,6, 7,8,9, 1,2,3) 
LHLangCustom <- LHLangCustom[order(LHLangCustom$PresOrder),]
LHLangCustom = arrange(LHLangCustom, ROIGroup)
makeRegionsBar(LHLangCustom)

MDRightCustom = filter(mystats, Group == 'MDRight', Task == 'JokesCustom')
MDRightCustom <- MDRightCustom[order(MDRightCustom$ROI),]
MDRightCustom = arrange(MDRightCustom, ROIGroup)
makeRegionsBar(MDRightCustom)

MDLeftCustom = filter(mystats, Group == 'MDLeft', Task == 'JokesCustom')
MDLeftCustom <- MDLeftCustom[order(MDLeftCustom$ROI),]
MDLeftCustom = arrange(MDLeftCustom, ROIGroup)
makeRegionsBar(MDLeftCustom)

