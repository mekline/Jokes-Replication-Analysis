#A separate script to analyze the results of the overlap analysis: within each ROI for each person, how much
#do the voxels of ToM as localized by bel>pho and ment>pain differ? 
library(dplyr)

ToMROI.Names = c('DM PFC', 'LTPJ',  'MM PFC', 'PC',
                 'RTPJ',  'VM PFC', 'RSTS');

overlaps <- read.csv('ToMfROIs_voxcount_ToM_Cloudy_20180314/spm_ss_overlap_data_clean.csv') %>%
  gather(key="rowID", value="nVoxels", -ROI.)%>%
  separate(rowID, c("subject", "subID", "comp"), extra = 'merge')%>%
  select(-subject)%>%
  mutate(comp = ifelse(comp == '1.1.', "size_fROI_belpho", comp))%>%
  mutate(comp = ifelse(comp == '2.2.', "size_fROI_mentpain", comp))%>%
  mutate(comp = ifelse(comp == '1.2.', "size_overlap", comp))%>%
  spread(comp, nVoxels)%>%
  mutate(size_region = size)%>%
  select(-size)%>%
  mutate(ROIname = ToMROI.Names[ROI.])%>%
  mutate(overlap = size_overlap/size_fROI_belpho)%>% #Justified because both localizers selected for top 10% voxels
  summarize(the_mean=mean(overlap))
  
  


