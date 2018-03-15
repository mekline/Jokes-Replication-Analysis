#Run this file to convert our toolbox outputs to tidy csv.  It's way easier on
#data from the PL2017 pipeline, but make sure you do the right one. 
#(Resulting data struct stored in variable myfile in case you want to access directly)

####
#Stuff to change!
myResultsFolder = paste(getwd(),'/ToMfROIS_resp_Cloudy_PL2017_20180312',sep='')
myOutputFolder = getwd()
myFilename = 'ToMfROIS_resp_Cloudy_PL2017_20180312.csv'
toSave = 1

nRowsToKeep = 7 #Needed for the newest version, corresponds to the number of regions/fROIs in your mask
  


#### VERSION FOR PL2017 DATA AS FORMATTED 3/12-3/14
#Leave the rest alone unless you're feeling fancy

library(dplyr)
library(tidyr)
library(stringr)

setwd(myResultsFolder)

#Open the weirdly formatted files and get just the table we want.
myfile  = read.csv('spm_ss_mROI_data.csv',sep=',', skip=1, header=TRUE)
lastsub = ncol(myfile)
myfile= myfile[1:nRowsToKeep,]#drop things past the individual % changes....

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
  setwd(myOutputFolder)
  zz <- file(myFilename, "w")
  write.csv(myfile, zz, row.names=FALSE)
  close(zz)
}

# #### Purportedly for PL2017; note that analyses 3/12-3/14 did NOT produce this new kind of output
# # Leave alone unless feeling fancy
# 
# 
# library(dplyr)
# library(tidyr)
# library(stringr)
# 
# setwd(myResultsFolder)
# 
# #Open the weirdly formatted files and get just the table we want.
# myfile  = read.csv('spm_ss_mROI_data.csv',sep=',', skip=1, header=FALSE)
# names(myfile) = c('ROIName','Subject','Contrast','nVoxels','SignalChange')
# 
# if(toSave){
#   setwd(myOutputFolder)
#   zz <- file(myFilename, "w")
#   write.csv(myfile, zz, row.names=FALSE)
#   close(zz)
# }

# #### Old version, pre PL2017
# #Leave the rest alone unless you're feeling fancy
# 
# library(dplyr)
# library(tidyr)
# library(stringr)
# 
# setwd(myResultsFolder)
# 
# #Open the weirdly formatted files and get just the table we want.
# myfile  = read.csv('spm_ss_mROI_data.csv',sep=',', skip=1, header=FALSE)
# lastsub = ncol(myfile)
# myfile= myfile[complete.cases(myfile[,lastsub]),]#drop things past the individual % changes....
# 
# #To add: Look at the # of ROI parcels and their sizes, declare this to be a particular
# #localizer, provide names for parcels. Also could add all that as an optional function arg.
# 
# extract_val <- function(mystring, mynum){# fn to extract subject & contrast numbers
#   foo = str_split(mystring, "\\.")
#   myval = unlist(foo[[1]][mynum])
#   return(myval)
#   
# }
# 
# #Make the data beautiful and longform.
# myfile[] <- lapply(myfile, as.character) #(Everything's a string, no factors)
# myfile <- myfile %>%
#   gather("Subject_and_Cont", "sigChange", Subject.1.1.:ncol(myfile)) %>%
#   rowwise() %>%
#   mutate(SubjectNumber = extract_val(Subject_and_Cont, 2)) %>%
#   mutate(Contrast = extract_val(Subject_and_Cont, 3)) %>%
#   select(-Subject_and_Cont) %>%
#   rename(ROI = ROI.)
# 
# 
# #Optional: print back out a nice file with a more informative name.
# if(toSave){
#   setwd(myOutputFolder)
#   zz <- file(myFilename, "w")
#   write.csv(myfile, zz, row.names=FALSE)
#   close(zz)
# }
