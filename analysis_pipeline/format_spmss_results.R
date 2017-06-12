#load_spmss_results
#
#This file loads the output of one of the results.csv files produced by the (mean signal) toolbox scripts into R.
#If I knew more about the mat file produced you could probably get all of this stuff out of
#there too.  But anyway this gets the mROI_data.csv file, sorts out its structure
#and reorganizes the data into proper longform. Take your analysis from there or save the result in a csv.
#Here, the csvs get saved back to the mean_signal folder for tidyness

####
#Stuff to change!
myResultsPath = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/meansignal_outputs/'
myOutputPath = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/meansignal_outputs/'

whichResults = 'ToMfROIS_resp_ToM_preliminary_20170612'; #Change this to the set of results to analyse!
toSave = 1

####
#Leave the rest alone unless you're feeling fancy

library(dplyr)
library(tidyr)
library(stringr)

setwd(paste(myResultsPath,whichResults, sep=""))

#Open the weirdly formatted files and get just the table we want. 
myfile  = read.csv('spm_ss_mROI_data.csv',sep=',', skip=1)
lastsub = ncol(myfile) 
myfile= myfile[complete.cases(myfile[,lastsub]),]#drop things past the individual % changes....

#To add: Look at the # of ROI parcels and their sizes, declare this to be a particular 
#localizer, provide names for parcels. Also could add all that as an optional function arg. 

extract_val <- function(mystring, mynum){# fn to extract subject & contrast numbers
  foo = str_split(mystring, "\\.")
  myval = unlist(foo[[1]][mynum])
  return(myval)
  
}

#Make the data beautiful and longform.
myfile[] <- lapply(myfile, as.character) #(Everything's a string, no factors)
myfile <- myfile %>% 
  gather("Subject_and_Cont", "sigChange", Subject.1.1.:ncol(myfile)) %>%
  rowwise() %>% 
  mutate(SubjectNumber = extract_val(Subject_and_Cont, 2)) %>%
  mutate(Contrast = extract_val(Subject_and_Cont, 3)) %>%
  select(-Subject_and_Cont) %>%
  rename(ROI = ROI.)
  

#Optional: print back out a nice file with a more informative name.
if(toSave){
  setwd(myOutputPath)
  myFileName = paste(whichResults,'.csv', sep="")
  zz <- file(myFileName, "w")
  write.csv(myfile, zz, row.names=FALSE)
  close(zz)
}