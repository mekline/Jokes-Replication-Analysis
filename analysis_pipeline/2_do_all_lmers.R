#This takes the individual-subject contrast values and runs some nifty lmer models.
rm(list = ls())
library(bootstrap)
library(dplyr)
library(ggplot2)
library(lme4)
library(pwr)
library(stringr)
library(tidyr)

#set wd
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")

#Make sure the data is loaded in!
load("allSigChange.RData")
#View(allSigChange)

#New! Write a function to wrap up anova results for pretty printing
LMER_results = data.frame(NULL)

LMERtoDF <- function(myanova, myfilterdata, name) {
  myfilter <- head(myfilterdata,1)
  myanova_df <- as.data.frame(myanova)
  myanova_df <- myanova_df%>%
    mutate(modelName = row.names(myanova_df),
           ROIMask = myfilter$ROIMask,
           localizer = myfilter$localizer,
           task = myfilter$task,
           testName = name)

  if (nrow(LMER_results) == 0) {
    print('get here!')
    LMER_results <- myanova_df
    print(LMER_results)
  }else{
    LMER_results <- rbind(LMER_results, myanova_df)
  }
  return(LMER_results)
}

# Linear mixed Models!
#Plan: Within each system (localizers, and jokes), test for basic localizer condition differences, then do some
#between-system comparisons


RHLangtoLang <- filter(allSigChange, ROIMask == "RHLang", localizer == 'Lang', task == "Lang", contrastName == 'S' | contrastName == 'N')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLangtoLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLangtoLang)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, RHLangtoLang, 'RHLangtoLang')


LHLangtoLang <- filter(allSigChange, ROIMask == "LHLang", localizer == 'Lang', task == "Lang", contrastName == 'S' | contrastName == 'N')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLangtoLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLangtoLang)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, LHLangtoLang, 'LHLangtoLang')

 

##MD to the MD task
MDRtoMD <- filter(allSigChange, ROIMask == "MDRight", localizer == 'MD', task == "MD", contrastName == 'H' | contrastName == 'E')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRtoMD)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRtoMD)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, MDRtoMD, 'MDRtoMD')


MDLtoMD <- filter(allSigChange, ROIMask == "MDLeft", localizer == 'MD', task == "MD", contrastName == 'H' | contrastName == 'E')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLtoMD)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLtoMD)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, MDLtoMD, 'MDLtoMD')


ToMtoToM <- filter(allSigChange, ROIMask == "ToM", localizer == 'ToM', task =='ToM', contrastName == 'bel' | contrastName == 'pho')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoToM)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoToM)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToMtoToM, 'ToMtoToM')

 
ToMtoCloudy <- filter(allSigChange, ROIMask == "ToM", localizer == 'ToM', task =='Cloudy', contrastName == 'ment' | contrastName == 'pain')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoCloudy)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoCloudy)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToMtoCloudy, 'ToMtoCloudy')



#To jokes! 

RHLang <- filter(allSigChange, ROIMask == "RHLang", localizer == 'Lang', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLang)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, RHLang, 'RHLangtoJokes')

 

LHLang <- filter(allSigChange, ROIMask == "LHLang", localizer == 'Lang', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLang)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, LHLang, 'LHLangtoJokes')

 

MDRight <- filter(allSigChange, ROIMask == "MDRight", localizer == 'MD', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRight)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRight)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, MDRight, 'MDRighttoJokes')

 

MDLeft <- filter(allSigChange, ROIMask == "MDLeft", localizer == 'MD', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLeft)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLeft)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, MDLeft, 'MDLefttoJokes')
 

ToM <- filter(allSigChange, ROIMask == "ToM", localizer == 'ToM', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToM)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToM)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM, 'ToMtoJokes')
 
Cloudy <- filter(allSigChange, ROIMask == "ToM", localizer == 'Cloudy', task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = Cloudy)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = Cloudy)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, Cloudy, 'CloudytoJokes')


######
#Then, do some comparisons between systems: ToM > MDRight and ToM RHLang, plus LHs for completeness

#Orig version conducted on E1
#ToM_MDRight <- filter(allSigChange, ROIMask == "ToM" | ROIMask == "MDRight", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
#m1 <- lmer(sigChange ~ contrastName*ROIMask + (contrastName|ROIName) + (contrastName*ROIMask|SubjectNumber), data = ToM_MDRight)
#m0 <- lmer(sigChange ~ contrastName+ROIMask + (contrastName|ROIName) + (contrastName*ROIMask|SubjectNumber), data = ToM_MDRight)
#anova(m1,m0)
 

#hypothesis: large between-system differences eat most of the variance.  Use joke-lit contrast value instead for the remainder
#(note this decision was made after E1, before E2)
ToM_MDRight_cont <- filter(allSigChange, (ROIMask == "ToM" & localizer == 'ToM') | (ROIMask == "MDRight" & localizer == "MD"), task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ ROIMask + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_MDRight_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_MDRight_cont)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM_MDRight_cont, 'Jokes-ToM-vs-MDRight')
 

ToM_RHLang_cont <- filter(allSigChange, (ROIMask == "ToM" & localizer == 'ToM')| (ROIMask == "RHLang" & localizer == 'Lang'), task =='Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ ROIMask + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_RHLang_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_RHLang_cont)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM_RHLang_cont, 'Jokes-ToM-vs-RHLang')
 

ToM_MDLeft_cont <- filter(allSigChange, (ROIMask == "ToM" & localizer == 'ToM') |  (ROIMask == "MDLeft" & localizer == "MD"), task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ ROIMask + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_MDLeft_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_MDLeft_cont)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM_MDLeft_cont, 'Jokes-ToM-vs-MDLeft')

 

ToM_LHLang_cont <- filter(allSigChange, (ROIMask == "ToM" & localizer == 'ToM') | (ROIMask == "LHLang" & localizer == "Lang"), task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ ROIMask + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_LHLang_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (ROIMask|SubjectNumber), data = ToM_LHLang_cont)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM_LHLang_cont, 'Jokes-ToM-vs-LHLang')

 
#Compare activations localized by classic (verbal) ToM localizer vs. Cloudy
ToM_Cloudy_cont <- filter(allSigChange, (ROIMask == "ToM" & localizer == "ToM") | (ROIMask == "ToM" & localizer == "Cloudy"), task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ localizer + (1|ROIName) + (localizer|SubjectNumber), data = ToM_Cloudy_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (localizer|SubjectNumber), data = ToM_Cloudy_cont)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToM_Cloudy_cont, 'Jokes-ToM-vs-Cloudy')


#####
#Finally, remodel ToM activations with funniness ratings
#NOTE we only preregistered doing this on the ToM results, hence other systems are in the exploratory section
#####
ToMCustom <- filter(allSigChange, ROIMask == "ToM", localizer == "ToM", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
ToMCustom$contrastName <- as.factor(ToMCustom$contrastName)[,drop=TRUE]
levels(ToMCustom$contrastName) <- c('low','med','high')

m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCustom)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToMCustom, 'ToMtoJokesCustom')

#And the same with Cloudy
ToMCloudyCustom <- filter(allSigChange, ROIMask == "ToM", localizer == "Cloudy", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
ToMCloudyCustom$contrastName <- as.factor(ToMCloudyCustom$contrastName)[,drop=TRUE]
levels(ToMCloudyCustom$contrastName) <- c('low','med','high')

m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCloudyCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCloudyCustom)
myanova = anova(m1,m0)
LMER_results <- LMERtoDF(myanova, ToMCloudyCustom, 'CloudytoJokesCustom')

#New! Print them all out beautifully
LMER_results <- LMER_results %>%
  select(c("testName","modelName", "Df","AIC","BIC","logLik","deviance","Chisq",
                                      "Chi Df","Pr(>Chisq)", "ROIMask","localizer","task"))%>%
  arrange(modelName, testName)


write.csv(LMER_results, "jokes_LMER_confirmatory.csv")