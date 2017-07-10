#This takes the individual-subject contrast values and runs some nifty lmer models.  First #many
#lines are reading in the contrasts as in localizer_t_tests, fun stuff starts on line 105

rm(list=ls(all=TRUE))
library(tidyr)
library(dplyr)
library(lme4)

#Set wd!
setwd("/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/analysis_pipeline")

#None of that nonsense, just make sure thw data is loaded in! If not, run 2figs_resp_jokes.R to at least line 108
View(allSigChange)


#########


#EFFECT SIZE CALCULATION! Requested by the journal.  There is no standard way to report effect sizes for linear mixed
#models, so the approach we'll take is to report mean signal change values at the system level.  This is calculated
#over in the figure script since we general those values there. 

# Linear mixed Models!
#Plan: Within each system (localizers, and jokes), test for basic localizer condition differences, then do some
#between-system comparisons


RHLangtoLang <- filter(allSigChange, Group == "RHLang", task == "Lang", contrastName == 'S' | contrastName == 'N')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLangtoLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLangtoLang)
anova(m1,m0)


LHLangtoLang <- filter(allSigChange, Group == "LHLang", task == "Lang", contrastName == 'S' | contrastName == 'N')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLangtoLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLangtoLang)
anova(m1,m0)
 

##MD to the MD task
MDRtoMD <- filter(allSigChange, Group == "MDRight", task == "MD", contrastName == 'H' | contrastName == 'E')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRtoMD)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRtoMD)
anova(m1,m0)

MDLtoMD <- filter(allSigChange, Group == "MDLeft", task == "MD", contrastName == 'H' | contrastName == 'E')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLtoMD)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLtoMD)
anova(m1,m0)

ToMtoToM <- filter(allSigChange, Group == "ToM", task =='ToM', contrastName == 'bel' | contrastName == 'pho')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoToM)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMtoToM)
anova(m1,m0)
 

#To jokes!

RHLang <- filter(allSigChange, Group == "RHLang", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = RHLang)
anova(m1,m0)
 

LHLang <- filter(allSigChange, Group == "LHLang", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLang)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = LHLang)
anova(m1,m0)
 

MDRight <- filter(allSigChange, Group == "MDRight", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRight)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDRight)
anova(m1,m0)
 

MDLeft <- filter(allSigChange, Group == "MDLeft", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLeft)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = MDLeft)
anova(m1,m0)
 

ToM <- filter(allSigChange, Group == "ToM", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToM)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToM)
anova(m1,m0)
 

######
#Then, do some comparisons between systems: ToM > MDRight and ToM RHLang, plus LHs for completeness

ToM_MDRight <- filter(allSigChange, Group == "ToM" | Group == "MDRight", task == 'Jokes', contrastName == 'joke' | contrastName == 'lit')
m1 <- lmer(sigChange ~ contrastName*Group + (contrastName|ROIName) + (contrastName*Group|SubjectNumber), data = ToM_MDRight)
m0 <- lmer(sigChange ~ contrastName+Group + (contrastName|ROIName) + (contrastName*Group|SubjectNumber), data = ToM_MDRight)
anova(m1,m0)
 

#hypothesis: large between-system differences eat most of the variance.  Use joke-lit contrast value instead
ToM_MDRight_cont <- filter(allSigChange, Group == "ToM" | Group == "MDRight", task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ Group + (1|ROIName) + (Group|SubjectNumber), data = ToM_MDRight_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (Group|SubjectNumber), data = ToM_MDRight_cont)
anova(m1,m0)
 

ToM_RHLang_cont <- filter(allSigChange, Group == "ToM" | Group == "RHLang", task =='Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ Group + (1|ROIName) + (Group|SubjectNumber), data = ToM_RHLang_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (Group|SubjectNumber), data = ToM_RHLang_cont)
anova(m1,m0)
 

ToM_MDLeft_cont <- filter(allSigChange, Group == "ToM" | Group == "MDLeft", task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ Group + (1|ROIName) + (Group|SubjectNumber), data = ToM_MDLeft_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (Group|SubjectNumber), data = ToM_MDLeft_cont)
anova(m1,m0)
 

ToM_LHLang_cont <- filter(allSigChange, Group == "ToM" | Group == "LHLang", task == 'Jokes', contrastName == 'joke-lit')
m1 <- lmer(sigChange ~ Group + (1|ROIName) + (Group|SubjectNumber), data = ToM_LHLang_cont)
m0 <- lmer(sigChange ~ 1 + (1|ROIName) + (Group|SubjectNumber), data = ToM_LHLang_cont)
anova(m1,m0)
 




#####
#Finally, remodel ToM activations with funniness ratings
ToMCustom <- filter(allSigChange, Group == "ToM", task == 'JokesCustom', contrastName == 'low' | contrastName == 'med' | contrastName == 'high')
#Make sure those factors are ordered....
ToMCustom$contrastName <- as.factor(ToMCustom$contrastName)
m1 <- lmer(sigChange ~ contrastName + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCustom)
m0 <- lmer(sigChange ~ 1 + (contrastName|ROIName) + (contrastName|SubjectNumber), data = ToMCustom)
anova(m1,m0)



####################HERE BE EXPLORATORY ANALYSES
#As is math, after powering the study up for the replication, we now detect (probably smaller) significant effects
#in all systems for jokes > nonjokes. The ToM ones are > RHLang and RMD (good!) but not significantly different in magnitude to 
#Lang or MDL.  One way to show that those MD and RHL activations are tapping something other than humor in the task would be if 
#funniness ratings didn't correlate with activation strength.  Let's see! (Oh wait, pause, this requires running more first level
#analyses to get those contrasts.  Check with Ev first. )
