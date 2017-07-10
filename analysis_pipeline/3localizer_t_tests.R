#This rebuilds the t tests that spmss spits out from the individual signal change values (reproduced here from ind. 
#signal change values so mk can track how those are done/feed into other analyses)

rm(list=ls(all=TRUE))
library(tidyr)
library(dplyr)
library(pwr)

#Set wd!
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")

#Make sure allSigChange is loaded. If it's not, run 2figs_resp_jokes.R to at least line 108
View(allSigChange)

#For the replication, commenting this out, we'll find out in a minute if any localizer-to-localizer
#measurements are not robust enough
#New 10/12: Localizer analysis shows that VMPFC localizer doesn't come out in this dataset, so remove it from
#the joke-lit tests for ToM and ToM custom (but leave it for the localizer itself)

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
# Report those T tests like we want for the paper
########

#Do corrections ever matter?
allTests <- allTests %>%
  mutate(sig = p < 0.05) %>%
  mutate(sigCor = p.adj < 0.05) %>%
  mutate(mismatch = sig != sigCor)

View(filter(allTests,mismatch))
#In the replication set, one mismatch: Nonwords over fixation in the linguistic task, LIFG orb, is 
#significant before but not after correction. We don't care about this bc the interesting thing from
#that task is Sentences - Nonwords. 

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
  filter(Group == 'LHLang-toLang', contrastName == 'sent-non') %>%
  summarise(n(), sum(sig), reportTests(t,p)) #Convention: when all significant, report the largest p

#allTests %>%
#  filter(Group == 'RHLang-toLang', contrastName == 'sent-non') %>%
#  summarise(n(), sum(sig), reportTests(t,p)) #found a surprise nonsig!
allTests %>%
  filter(Group == 'RHLang-toLang', contrastName == 'sent-non', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p)) 
filter(allTests, Group == 'RHLang-toLang', contrastName == 'sent-non', !sig)

###MD localizer check
allTests %>%
  #filter(Group == 'MDRight-toLang', contrastName == 'non-sent') %>%
  #summarise(n(), sum(sig), reportTests(t,p))
  filter(Group == 'MDRight-toLang', contrastName == 'non-sent', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p)) 
filter(allTests, Group == 'MDRight-toLang', contrastName == 'non-sent', !sig)

allTests %>%
  filter(Group == 'MDLeft-toLang', contrastName == 'non-sent', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDLeft-toLang', contrastName == 'non-sent', !sig)


allTests %>%
  filter(Group == 'ToM-toToM', contrastName == 'bel-pho', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p)) 
filter(allTests, Group == 'ToM-toToM', contrastName == 'bel-pho', !sig)


###
#RESP JOKES

### RHLang
#Jokes and Nonjokes both activate, but no differences.

allTests %>%
  filter(Group == 'RHLang', contrastName == 'joke', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'RHLang', contrastName == 'joke', !sig)

allTests %>%
  filter(Group == 'RHLang', contrastName == 'lit', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'RHLang', contrastName == 'lit', !sig)

allTests %>%
  filter(Group == 'RHLang', contrastName == 'joke-lit') %>%
  summarise(n(), sum(sig), reportTests(t,p)) 


### LHLang
#Jokes and Nonjokes both activate, but no differences.

allTests %>%
  filter(Group == 'LHLang', contrastName == 'joke') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'LHLang', contrastName == 'lit') %>%
  summarise(n(), sum(sig), reportTests(t,p))

allTests %>%
  filter(Group == 'LHLang', contrastName == 'joke-lit', !sig) %>%
  summarise(n(), sum(sig), reportTests(t,p)) #ONLY ONE of the ROIs significant
filter(allTests, Group == 'LHLang', contrastName == 'joke-lit', sig)


### RHMD
# RH is pretty boring

allTests %>%
  filter(Group == 'MDRight', contrastName == 'joke', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'joke', !sig)

allTests %>%
  filter(Group == 'MDRight', contrastName == 'lit',sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'lit', !sig)

allTests %>%
  filter(Group == 'MDRight', contrastName == 'joke-lit', !sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDRight', contrastName == 'joke-lit', sig)


###LHMD
# LH has some joke-lit differences

allTests %>%
  filter(Group == 'MDLeft', contrastName == 'joke', sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDLeft', contrastName == 'joke', !sig)

allTests %>%
  filter(Group == 'MDLeft', contrastName == 'lit',sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDLeft', contrastName == 'lit', !sig)

allTests %>%
  filter(Group == 'MDLeft', contrastName == 'joke-lit', !sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'MDLeft', contrastName == 'joke-lit', sig)



### ToM
# Interesting activations!
allTests %>%
  filter(Group == 'ToM', contrastName == 'joke',!sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'joke', sig)

allTests %>%
  filter(Group == 'ToM', contrastName == 'lit', !sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'lit', sig)

allTests %>%
  filter(Group == 'ToM', contrastName == 'joke-lit', !sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'joke-lit', sig)

#10/14 Huh, where did the ToM paramfun test go? Here it is again...
allTests %>%
  filter(Group == 'ToMCustom', contrastName == 'paramfun',!sig) %>%
  summarise(n(), sum(sig), reportTests(t,p))
filter(allTests, Group == 'ToM', contrastName == 'joke-lit', sig)


###############Here be exploratory analyses######

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
  filter(ROIName %in% c('RTPJ','LTPJ','PC','MMPFC') ) %>%
  group_by(ROIName)%>%
  summarise(m = mean(sigChange), sd = sd(sigChange), t = t.test(sigChange, mu=0,alt='greater')$statistic, 
            p = t.test(sigChange, mu=0,alt='greater')$p.value)

forPower$n <- 12
forPower$cohens_d <- forPower$m / forPower$sd

ptests <- mapply(pwr.t.test, n=forPower$n, d=forPower$cohens_d, sig.level=0.05, alternative='greater')

#These effects are powered okay: range 0.587 - 0.856

#Assume the smallest effect in ToM regions is the true effect size
effect_est <- min(forPower$cohens_d)

#How many participants do we need for 80% power at p=0.05?
pwr.t.test(d=effect_est, sig.level=0.05, power = 0.80, alternative='greater')

#21 participants!