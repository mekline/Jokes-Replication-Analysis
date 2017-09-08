#This rebuilds the t tests that spmss spits out from the individual signal change values (reproduced here from ind. 
#signal change values so mk can track how those are done/feed into other analyses)

#Prerequisites to run this file
#- Set (your) working directory
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")

#Make sure allSigChange is loaded. If it's not, run 2figs_resp_jokes.R to at least line 108
View(allSigChange)

#For the replication, commenting this out, we'll find out in a minute if any localizer-to-localizer
#measurements are not robust enough
#Result 10/12: Localizer analysis shows that VMPFC localizer DOESN'T come out in this dataset (replication/study 2), so DONT remove it from
#the joke-lit tests for ToM and ToM custom 

#allSigChange = allSigChange %>%
#  filter(!(Group == 'ToM' & ROIName =='VMPFC')) %>%
#  filter(!(Group == 'ToMCustom' & ROIName =='VMPFC'))


#######
# Calculate T Tests
#######

allTests <- allSigChange %>%
  group_by(Group, task)%>%
  summarize(familySize = length(unique(ROI))) %>%
  merge(allSigChange) %>%
  group_by(Group, task, ROI, ROIName, contrastName, familySize) %>%
  summarise(t = t.test(sigChange, mu=0,alt='greater')$statistic, 
            p = t.test(sigChange, mu=0,alt='greater')$p.value) %>%
  ungroup()%>%
  group_by(Group, contrastName)%>%
  mutate(p.adj = p.adjust(p, method="fdr", n=familySize[1]))%>%
  ungroup()

View(allTests)
zz = file('localizer_t_tests_all.csv', 'w')
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
  filter(Group == 'LHLang', task == 'Lang', contrastName == 'S-N') %>%
  summarise(n(), sum(sig), reportTests(t,p)) #Convention: when all significant, report the largest p

allTests %>%
  filter(Group == 'RHLang', task == 'Lang', contrastName == 'S-N') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 

allTests %>%
  filter(Group == 'MDLeft', task == 'MD', contrastName == 'H-E') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'MDRight', task == 'MD', contrastName == 'H-E') %>%
  summarise(n(), sum(sig), reportTests(t,p)) #Orig found a surprise nonsig, but not in the replication

#(Note, in the orig we evaluated MD localizer with non-sent, but now we have participants with 2 localizer sessions!)

allTests %>%
  filter(Group == 'ToM', task == 'ToM', contrastName == 'bel-pho') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 

###
#RESP JOKES

### RHLang
#Jokes and Nonjokes both activate, and this time differences! RAngG is nonsignificant

allTests %>%
  filter(Group == 'RHLang', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'RHLang', contrastName == 'joke', !sig)
filter(allTests, Group == 'RHLang', contrastName == 'joke', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'RHLang', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'RHLang', contrastName == 'lit', !sig)
filter(allTests, Group == 'RHLang', contrastName == 'lit', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'RHLang', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 


### LHLang
#Jokes and Nonjokes both activate, and this time there's differences?! 

allTests %>%
  filter(Group == 'LHLang', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'LHLang', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'LHLang', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

### RHMD

allTests %>%
  filter(Group == 'MDRight', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'joke', !sig)

allTests %>%
  filter(Group == 'MDRight', task == 'Jokes',  contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'lit', !sig)

allTests %>%
  filter(Group == 'MDRight', task == 'Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'joke-lit', !sig)
filter(allTests, Group == 'MDRight', contrastName == 'joke-lit', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))


###LHMD

allTests %>%
  filter(Group == 'MDLeft', task == 'Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'MDLeft', task == 'Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDLeft', contrastName == 'lit', !sig)

allTests %>%
  filter(Group == 'MDLeft', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))


### ToM
# Interesting activations!
allTests %>%
  filter(Group == 'ToM', task =='Jokes', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'joke', sig)

allTests %>%
  filter(Group == 'ToM', task =='Jokes', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'lit', sig)

allTests %>%
  filter(Group == 'ToM', task =='Jokes', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

#Paramfun t test (jokesCustom parametric)
allTests %>%
 filter(Group == 'ToM', task == 'JokesCustom', contrastName == 'linear') %>%
 summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'joke-lit', sig)


################ Exploratory analysis from Study 1: power analysis for Study 2


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
  filter(Group == 'ToM', contrastName == 'joke-lit') %>%
  filter(ROIName %in% c('RTPJ','LTPJ','PC','MM PFC') ) %>%
  group_by(ROIName)%>%
  summarise(m = mean(sigChange), sd = sd(sigChange), t = t.test(sigChange, mu=0,alt='greater')$statistic, 
            p = t.test(sigChange, mu=0,alt='greater')$p.value)

forPower$n <- 12
forPower$cohens_d <- forPower$m / forPower$sd

ptests <- mapply(pwr.t.test, n=forPower$n, d=forPower$cohens_d, sig.level=0.05, alternative='greater')

#These effects are powered okay: range 0.587 - 0.856
#UPDATE: Study 2, the effects are powered VERY well, RTPJ, LTPJ, PC are at .92 or higher, MM PFC is at .62


#Assume the smallest effect in ToM regions is the true effect size
effect_est <- min(forPower$cohens_d)

#How many participants do we need for 80% power at p=0.05?
pwr.t.test(d=effect_est, sig.level=0.05, power = 0.80, alternative='greater')

#21 participants!