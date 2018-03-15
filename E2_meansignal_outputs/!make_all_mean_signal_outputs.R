#As of 3/14, moving this to the meansignals file where it belongs!
#
# This does not need to be run to reproduce the analyses in the Jokes paper, unless you want to recapitulate
# the transformation from SPM_SS output folders to raw csv. 
#
#This file loads the output of one of the results.csv files produced by the (mean signal) toolbox scripts into R.
#If I knew more about the mat file produced you could probably get all of this stuff out of
#there too.  But anyway this gets the mROI_data.csv file, sorts out its structure
#and reorganizes the data into proper longform, and makes a big csv.  Note the different procedure for the
#PL2017 pipeline ones, we have to use a slightly differnt process!



library(dplyr)
library(tidyr)
library(stringr)
requireNamespace(plyr)

####
#Stuff to change!
myResultsPath = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/E2_meansignal_outputs/'
myOutputPath = '/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/E2_meansignal_outputs/'

whichResults = c('LangfROIs_resp_JokesCustom_20170720',
                 'LangfROIs_resp_JokesCustom_Top50Voxels_20170720',
                 'LangfROIs_resp_Jokes_20170703',
                 'LangfROIs_resp_Jokes_Top50Voxels_20170703',
                 'LangfROIs_resp_Lang_20170703',
                 'MDfROIs_resp_JokesCustom_20170726',
                 'MDfROIs_resp_JokesCustom_Top50Voxels_20170720',
                 'MDfROIs_resp_Jokes_20170720',
                 'MDfROIs_resp_Jokes_Top50Voxels_20170720',
                 'MDfROIs_resp_MD_20170720',
                 'RHLfROIs_resp_JokesCustom_20170720',
                 'RHLfROIs_resp_JokesCustom_Top50Voxels_20170720',
                 'RHLfROIs_resp_Jokes_20170703',
                 'RHLfROIs_resp_Jokes_Top50Voxels_20170703',
                 'RHLfROIs_resp_Lang_20170703',
                 'ToMfROIS_resp_JokesCustom_20170720',
                 'ToMfROIS_resp_JokesCustom_Top50Voxels_20170720',
                 'ToMfROIS_resp_Jokes_20170720',
                 'ToMfROIS_resp_Jokes_Top50Voxels_20170720',
                 'ToMfROIS_resp_ToM_20170720',
                 'SplitHalf_RHLfROIs_resp_Jokes_20170904',
                 'SplitHalf_LangfROIs_resp_Jokes_20170904',
                 'SplitHalf_MDfROIs_resp_Jokes_20170904',
                 'SplitHalf_ToMfROIs_resp_Jokes_20170904');

PL2017Results = c('ToMfROIs_resp_Cloudy_PL2017_20180312',
                  'CloudyToMfROIS_mentpain_resp_JokesCustom_PL2017_20180314',
                  'CloudyToMfROIS_mentpain_resp_Jokes_PL2017_20180314');


toSave = 1 

  ####
  #Leave the rest alone unless you're feeling fancy

all_mean_signal = data.frame(NULL)

for (result in whichResults){
  setwd(paste(myResultsPath,result, sep=""))
  
  #Open the weirdly formatted files and get just the table we want. 
  myfile  = read.csv('spm_ss_mROI_data.csv',sep=',', skip=1)
  lastsub = ncol(myfile) 
  myfile= myfile[complete.cases(myfile[,lastsub]),]#drop things past the individual % changes....
  
  #To add: Look at the # of ROI parcels and their sizes, declare this to be a particular 
  #localizer, provide names for parcels. Also could add all that as an optional function arg. 
  #(this happens in 2_figs etc. now, but we do read the filenames to make that easier...)
  
  #Add details about what this analysis is by splitting up the filename (requires regular filenames!)
  rundetails = str_split_fixed(result, '_', 4)
  myfROIs = rundetails[[1]]
  myTask = rundetails[[3]]
  myMethod = 'Top10Percent'
  if(str_detect(rundetails[[4]], 'Top50')){myMethod = 'Top50Voxels'}
  
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
    rename(ROI = ROI.) %>%
    mutate(filename = result)%>%
    mutate(fROIs = myfROIs)%>%
    mutate(task = myTask)%>%
    mutate(ind_selection_method = myMethod)%>%
    plyr::rename(replace = c(average.ROI.size="ROI.size"), warn_missing = FALSE)
  
  
  myfile$pipeline = 'Old_evlab_pipeline'
  #Optional: print back out a nice file with a more informative name.
  if(toSave){
    setwd(myOutputPath)
    myFileName = paste(result,'.csv', sep="")
    zz <- file(myFileName, "w")
    write.csv(myfile, zz, row.names=FALSE)
    close(zz)
  }

  #And add it to the giant dataframe
  if (nrow(all_mean_signal) == 0){
    all_mean_signal = myfile
  }else{
  all_mean_signal = rbind(all_mean_signal, myfile)
  }
}

#Now do the PL2017 ones...
for (result in PL2017Results){
  #Get the csv produced by convert_spmss_results.
  setwd(myResultsPath)
  mycsvfile = read.csv(paste(myResultsPath, result, '.csv', sep=""))
  mycsvfile$filename = result
  result = str_replace(result, 'mentpain_','')#Small fix to remove ment-pain string from Cloudy filenames
  mydetails = str_split_fixed(result, '_', 4)
  mycsvfile$fROIs <- mydetails[[1]] 
  mycsvfile$task <- mydetails[[3]] 
  mycsvfile$ind_selection_method <- 'Top10Percent' #All use standard voxel selection
  
  mycsvfile$pipeline = 'PL2017'

  #Optional: print back out a nice file (replaces convert_spmss_results, with added extra columns)
  if(toSave){
    setwd(myOutputPath)
    myFileName = paste(result,'.csv', sep="")
    zz <- file(myFileName, "w")
    write.csv(myfile, zz, row.names=FALSE)
    close(zz)
  }
  
  #And add it to the giant dataframe
  if (nrow(all_mean_signal) == 0){
    all_mean_signal = mycsvfile
  }else{
    all_mean_signal = rbind(all_mean_signal, mycsvfile)
  }
  
}

setwd(myOutputPath)
write.csv(all_mean_signal, 'all_mean_signal_outputs.csv', row.names = FALSE)
