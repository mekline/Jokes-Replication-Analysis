#Prerequisites to run this file
#- Set (your) working directory
#ALL packages necessary for the analysis pipeline should get loaded here
rm(list = ls())
library(bootstrap)
library(dplyr)
library(ggplot2)
library(lme4)
library(pwr)
library(stringr)
library(tidyr)

setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")
mywd = getwd()

#Make sure allSigChange is loaded. If it's not, load it
load("allSigChange.RData")
View(allSigChange)

#NOTE: Result 10/12: Localizer analysis shows that VMPFC localizer DOESN'T fail 
#in this dataset (replication/study 2), so DONT remove it from
#the joke-lit tests for ToM and ToM custom analysis (this is diff from E1)

#Quirk to fix - below, we'll want Cloudy's ment-pain contrast, and we 
#didn't calculate that directly in the secondary (for no good reason.)  Fix/add it here:

CloudyinToM <- allSigChange%>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task == 'Cloudy') %>%
  filter(contrastName == 'ment' | contrastName == 'pain') %>%
  spread(contrastName, sigChange) %>%
  mutate(sigChange = ment-pain) %>%
  select(-c(ment, pain)) %>%
  mutate(contrastName = 'ment-pain')

allSigChange <- bind_rows(allSigChange, CloudyinToM)
#######
# Calculate T Tests
#######

allTests <- allSigChange %>%
  group_by(ROIMask, localizer, task)%>%
  summarize(familySize = length(unique(ROIName))) %>%
  merge(allSigChange) %>%
  group_by(ROIMask, localizer, task, ROIName, contrastName, familySize) %>%
  summarise(t = t.test(sigChange, mu=0,alt='greater')$statistic, 
            p = t.test(sigChange, mu=0,alt='greater')$p.value) %>%
  ungroup()%>%
  group_by(ROIMask, localizer, task, contrastName)%>%
  mutate(p.adj = p.adjust(p, method="fdr"))%>%
  mutate(howmany = length(p)) %>%
  ungroup()

View(allTests)
zz = file('jokes_t_tests_all.csv', 'w')
write.csv(allTests, zz, row.names=FALSE)
close(zz)

########
# Report those T tests like we want for the paper (main text)
########

#Do corrections ever matter?
allTests <- allTests %>%
  mutate(sig = p < 0.05) %>%
  mutate(sigCor = p.adj < 0.05) %>%
  mutate(mismatch = sig != sigCor)

View(filter(allTests,mismatch))
#In the replication set, one mismatch: Nonwords over fixation in the linguistic task, LIFG orb, is 
#significant before but not after correction. We don't care about this bc the interesting thing from
#that task is Sentences - Nonwords, not activation over fixation. 

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

###
#RESP LOCALIZER
allTests %>%
  filter(ROIMask == 'LHLang', localizer == 'Lang', task == 'Lang', contrastName == 'S-N') %>%
  summarise(n(), sum(sig), reportTests(t,p)) #Convention: when all significant, report the largest p

allTests %>%
  filter(ROIMask == 'RHLang', localizer == 'Lang', task == 'Lang', contrastName == 'S-N') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 

allTests %>%
  filter(ROIMask == 'MDLeft', localizer == 'MD', task == 'MD', contrastName == 'H-E') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'MDRight', localizer == 'MD', task == 'MD', contrastName == 'H-E') %>%
  summarise(n(), sum(sig), reportTests(t,p)) #Orig found a surprise nonsig, but not in the replication

#(Note, in the orig we evaluated MD localizer with non-sent, but now we have participants with 2 localizer sessions!)

allTests %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task == 'ToM', contrastName == 'bel-pho') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 

#NEW: Cloudy localizer!
allTests %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task == 'Cloudy', contrastName == 'ment-pain') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 

###
#RESP JOKES

### RHLang
#Jokes and Nonjokes both activate, and this time differences! RAngG is nonsignificant

allTests %>%
  filter(ROIMask == 'RHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'RHLang', localizer == 'Lang', contrastName == 'joke', !sig)
filter(allTests, ROIMask == 'RHLang', localizer == 'Lang', contrastName == 'joke', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'RHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'RHLang', localizer == 'Lang', contrastName == 'lit', !sig)
filter(allTests, ROIMask == 'RHLang', localizer == 'Lang', contrastName == 'lit', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'RHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 


### LHLang
#Jokes and Nonjokes both activate, and this time there's differences?! 

allTests %>%
  filter(ROIMask == 'LHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'LHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'LHLang', localizer == 'Lang', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

### RHMD

allTests %>%
  filter(ROIMask == 'MDRight', localizer == 'MD', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'MDRight', contrastName == 'joke', !sig)

allTests %>%
  filter(ROIMask == 'MDRight', localizer == 'MD', task == 'Jokes',  contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'MDRight', contrastName == 'lit', !sig)

allTests %>%
  filter(ROIMask == 'MDRight', localizer == 'MD', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'MDRight', localizer == 'MD', contrastName == 'joke-lit', !sig)
filter(allTests, ROIMask == 'MDRight', localizer == 'MD', contrastName == 'joke-lit', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))


###LHMD

allTests %>%
  filter(ROIMask == 'MDLeft', localizer == 'MD', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(ROIMask == 'MDLeft', localizer == 'MD', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'MDLeft', contrastName == 'lit', !sig)

allTests %>%
  filter(ROIMask == 'MDLeft', localizer == 'MD', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))


### ToM
# Interesting activations!
allTests %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task =='Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'ToM', contrastName == 'joke', sig)

allTests %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task =='Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'ToM', contrastName == 'lit', sig)

allTests %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', task =='Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

#Paramfun t test (jokesCustom parametric)
allTests %>%
 filter(ROIMask == 'ToM', localizer == 'ToM', task == 'JokesCustom', contrastName == 'linear') %>%
 summarise(n(), sum(sig), reportTests(t,p))


#Cloudy (New!) - All significant on the key contrast
allTests %>%
  filter(ROIMask == 'ToM', localizer == 'Cloudy', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, ROIMask == 'ToM', localizer == 'Cloudy', contrastName == 'joke', sig)

allTests %>%
  filter(ROIMask == 'ToM', localizer == 'Cloudy', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
#all significantly *negative*, fascinating. 

allTests %>%
  filter(ROIMask == 'ToM', localizer == 'Cloudy', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

#And Cloudy on the parametric analysis for Custom assignments, noting that we weirdly named it 'paramfun' this time
allTests %>%
  filter(ROIMask == 'ToM', localizer == 'Cloudy', task == 'JokesCustom', contrastName == 'paramfun') %>%
  summarise(n(), sum(sig), reportTests(t,p))


################ Exploratory analysis from Study 1: doing the power analysis to design Study 2


#Let's try and do a power analysis on the Jokes results. (Considering a replication
#since a journal has asked for one) There are 4 regions we expect to 
#find differences in: RTPJ, LTPJ, MMPFC, PC.  How big are those differences and how
#well powered are we?

cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}

forPower <- allSigChange %>%
  filter(ROIMask == 'ToM', localizer == 'ToM', contrastName == 'joke-lit') %>%
  filter(ROIName %in% c('RTPJ','LTPJ','PC','MM PFC') ) %>%
  group_by(ROIName)%>%
  summarise(m = mean(sigChange), sd = sd(sigChange), t = t.test(sigChange, mu=0,alt='greater')$statistic, 
            p = t.test(sigChange, mu=0,alt='greater')$p.value)

forPower$n <- 21 #FOR E2
forPower$cohens_d <- forPower$m / forPower$sd

ptests <- mapply(pwr.t.test, n=forPower$n, d=forPower$cohens_d, sig.level=0.05, alternative='greater')

#For E1, These effects are powered okay: range 0.587 - 0.856
#UPDATE: Study 2, the effects are powered VERY well, RTPJ, LTPJ, PC are at .92 or higher, MM PFC is at .62


#(From E1)
#Assume the smallest effect in ToM regions is the true effect size
#effect_est <- min(forPower$cohens_d)

#How many participants do we need for 80% power at p=0.05?
#pwr.t.test(d=effect_est, sig.level=0.05, power = 0.80, alternative='greater')
#21 participants!