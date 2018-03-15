
#########

#EFFECT SIZE CALCULATION! Requested by the journal after E1.  There is no standard way to report effect sizes for linear mixed
#models, so the approach we'll take is to report mean signal change values at the system level.  This is calculated
#over in the figure script (2figs_resp_jokes) since we generated those values there. 
#########

#set wd
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")

#Make sure the data is loaded in!
load("allSigChange.RData")
#View(allSigChange)


# Generate mystats - a somewhat more comprehensive table of summary statistics

#Get Jokes values
JokeSigs = allSigChange %>%
  filter(contrastName %in% c('joke','lit','high','med','low'))

#Next, get the table that we'll be making the graphs from: for each region (including the average region), take all 
#the individual signal changes and calculate a mean and a standard error
sterr <- function(mylist){
  my_se = sd(mylist)/sqrt(length(mylist)) 
  
  return(my_se)
}

mystats = aggregate(JokeSigs$sigChange, by=list(JokeSigs$ROIMask, JokeSigs$localizer, JokeSigs$task, JokeSigs$ROIName, JokeSigs$contrastName), mean)
names(mystats) = c('ROIMask','localizer', 'task', 'ROIName', 'contrastName', 'themean')
myster = aggregate(JokeSigs$sigChange, by=list(JokeSigs$ROIMask, JokeSigs$localizer, JokeSigs$task, JokeSigs$ROIName, JokeSigs$contrastName), sterr)
names(myster) = c('ROIMask','localizer', 'task', 'ROIName', 'contrastName', 'sterr')

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
mybootup = aggregate(JokeSigs$sigChange, by=list(JokeSigs$ROIMask, JokeSigs$localizer, JokeSigs$task, JokeSigs$ROIName, JokeSigs$contrastName), bootup)
names(mybootup) = c('ROIMask','localizer', 'task', 'ROIName', 'contrastName',  'bootup')
mybootdown = aggregate(JokeSigs$sigChange, by=list(JokeSigs$ROIMask, JokeSigs$localizer, JokeSigs$task, JokeSigs$ROIName, JokeSigs$contrastName), bootdown)
names(mybootdown) = c('ROIMask','localizer', 'task', 'ROIName', 'contrastName',  'bootdown')

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
  select(c("ROIMask", "localizer", "contrastName", "themean")) %>%
  spread(contrastName, themean) %>%
  mutate(sigChange = joke-lit)

