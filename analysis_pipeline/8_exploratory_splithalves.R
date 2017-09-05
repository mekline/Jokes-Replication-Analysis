#This is a separate file that JUST does the split-half analyses. We recapitulate the cleaning/processing
#code here just to avoid infecting the main scripts with the split-half parts of the data. Interesting
#stuff starts on line 93

#(((EXPLORATORY F)))


rm(list = ls())
library(bootstrap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)


setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
meansig_outputs_folder = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/meansignal_outputs/'

########
#READ IN DATA
########

allSigChange = read.csv(paste(meansig_outputs_folder, 'all_mean_signal_outputs.csv', sep=''))

allSigChange = allSigChange %>%
  filter(ind_selection_method == 'Top10Percent') %>%
  filter(filename %in% c('SplitHalf_RHLfROIs_resp_Jokes_20170904', 
                                                     'SplitHalf_LangfROIs_resp_Jokes_20170904',
                                                     'SplitHalf_MDfROIs_resp_Jokes_20170904',
                                                     'SplitHalf_ToMfROIs_resp_Jokes_20170904')) %>%
  mutate(task = 'Jokes') %>%  #Oops, the parser messed up these names, fix'em!
  mutate(fROIs = ifelse(filename == 'SplitHalf_RHLfROIs_resp_Jokes_20170904', 'RHLfROIs', 
                        ifelse(filename == 'SplitHalf_LangfROIs_resp_Jokes_20170904', 'LangfROIs',
                               ifelse(filename == 'SplitHalf_MDfROIs_resp_Jokes_20170904', 'MDfROIs', 'ToMfROIs'))))

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

splithalf.contrasts = c('ODD_joke-lit', 'EVEN_joke-lit')

#Split the data into groups by fROIs, and rename them as appropriate
RHLang_sigs = data.frame(NULL)
LHLang_sigs = data.frame(NULL)
MD_sigs = data.frame(NULL)
ToM_sigs = data.frame(NULL)


RHLang_sigs = allSigChange %>%
  filter(fROIs == 'RHLfROIs')%>%
  mutate(ROIName = RHLangROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = splithalf.contrasts[Contrast]) %>%
  mutate(Group = 'RHLang') %>%
  ungroup()

LHLang_sigs = allSigChange %>%
  filter(fROIs == 'LangfROIs')%>%
  mutate(ROIName = LangROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = splithalf.contrasts[Contrast]) %>%
  mutate(Group = 'LHLang') %>%
  ungroup()

MD_sigs = allSigChange %>%
  filter(fROIs == 'MDfROIs')%>%
  mutate(ROIName = MDROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = splithalf.contrasts[Contrast]) %>%
  mutate(Group = ifelse(ROI %%2 == 1, 'MDLeft','MDRight')) %>%
  ungroup()

ToM_sigs = allSigChange %>%
  filter(fROIs == 'ToMfROIs')%>% ##Typo in all the filenames!
  mutate(ROIName = ToMROI.Names[ROI]) %>%
  group_by(task)%>%
  mutate(contrastName = splithalf.contrasts[Contrast]) %>%
  mutate(Group = 'ToM') %>%
  ungroup()

#And stick it all back together!!
splitHalfSigChange = rbind(RHLang_sigs, LHLang_sigs, MD_sigs, ToM_sigs)

#(No average - by - network needed for these analyses.)

##################
#Now, to do some split half visualizations and analyses. 
##################
#get the data in shape for some correlations! That means: each observation (human) has 10 * nROI values - odd and even in each fROI in each region

splitHalfSigChange = splitHalfSigChange %>%
  select(c(SubjectNumber, Group, ROIName, contrastName, sigChange)) %>%
  unite(myEntry, Group, ROIName, contrastName, sep=".") %>%
  spread(myEntry, sigChange) %>%
  select(-c(SubjectNumber))


#World's biggest correlation matrix! Each humans' numbers get correlated here. 
#Whoa! We'll be doing some subsetting...this sure is dumb since I unified the columns above, but oh well. 
cormat <- cor(splitHalfSigChange, use='pairwise.complete.obs')
melted_cormat <- melt(cormat) %>%
  separate(Var1, into = c('Group1','ROIName1','contrastName1'), sep = '\\.') %>%
  separate(Var2, into = c('Group2','ROIName2','contrastName2'), sep = '\\.')

#Rule: only compare across odds and evens, not fROI_12 at ODD and fROI_13 and ODD. 
#Then, average those two values together. This lets us fairly compare
#self-to-self correlations with self-to-other correlations
odd_even <- melted_cormat %>%
  filter(contrastName1 != contrastName2) %>%
  select(one_of('Group1','Group2','ROIName1','ROIName2','value')) %>%
  group_by(Group1, Group2, ROIName1, ROIName2) %>%
  summarise(meancorr = mean(value))

#odd_even is the *mean across people* at each individual ROI 
  
ggplot(data = odd_even, aes(x=ROIName1, y=ROIName2, fill=meancorr)) + 
  geom_tile() +
  facet_wrap(Group1 ~ Group2, scales="free")

#Cool, that's giant and pretty useless to look at. How to actually represent this in real life? 
#We want to know whether average within-system correlations are higher than between-system correlations,
#And also we want to know whether some systems' correlations are higher than others. 

#So, the nice way to visualize this and then measure it would be:
#Visualize: Get 1 value per system-pair, i.e. average all the ToMOdd-RHLangEven comparisons and so on

system_avg = odd_even %>%
  group_by(Group1, Group2) %>%
  summarise(meancorr = mean(meancorr))

#We'd like to put hemispheres of systems together here, since they are actually correlated!
system_avg$Group1 <-factor(system_avg$Group1, levels = c('RHLang','LHLang','MDRight','MDLeft','ToM'))
system_avg$Group2 <-factor(system_avg$Group2, levels = c('RHLang','LHLang','MDRight','MDLeft','ToM'))

ggplot(data = system_avg, aes(x=Group1, y=Group2, fill=meancorr)) + 
  geom_tile()

#Measure: We have a set of system-pair vectors. Compare them, probably not with parametric tests? I'm
#not sure, we'll plot them to check. 

  
  
