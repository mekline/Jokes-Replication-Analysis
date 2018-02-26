
experiments(1)=struct(...
    'name','loc',...% language localizer 
    'pwd1','/mindhive/evlab/u/Shared/SUBJECTS/',...
    'pwd2','model_BiLing',...
    'data',{{ ...
        '380_FED_20160407b_3T1_PL2017'
        '381_FED_20160407c_3T1_PL2017'
        '383_FED_20160505a_3T1_PL2017'
        '406_FED_20160601a_3T1_PL2017'
        '413_FED_20160707c_3T1_PL2017'
        '412_FED_20160707a_3T1_PL2017'
        '179_FED_20160706b_3T1_PL2017'
        '415_FED_20160714a_3T2_PL2017'
        '442_FED_20161004b_3T1_PL2017'
        '456_FED_20161019b_3T1_PL2017'
        '389_FED_20170111a_3T2_PL2017'
        '530_FED_20170114a_3T2_PL2017'
        '534_FED_20170119b_3T2_PL2017'
        '591_FED_20170422d_3T2_PL2017'
        '587_FED_20170423a_3T2'
        '593_FED_20170423c_3T2_PL2017'
        }}); % subject IDs
experiments(2)=struct(...
    'name','crit',...% non-lang expt
    'pwd1','/mindhive/evlab/u/Shared/SUBJECTS/',...
    'pwd2','model_BiLing',...
    'data',{{ ...
        '380_FED_20160407b_3T1_PL2017'
        '381_FED_20160407c_3T1_PL2017'
        '383_FED_20160505a_3T1_PL2017'
        '406_FED_20160601a_3T1_PL2017'
        '413_FED_20160707c_3T1_PL2017'
        '412_FED_20160707a_3T1_PL2017'
        '179_FED_20160706b_3T1_PL2017'
        '415_FED_20160714a_3T2_PL2017'
        '442_FED_20161004b_3T1_PL2017'
        '456_FED_20161019b_3T1_PL2017'
        '389_FED_20170111a_3T2_PL2017'
        '530_FED_20170114a_3T2_PL2017'
        '534_FED_20170119b_3T2_PL2017'
        '591_FED_20170422d_3T2_PL2017'
        '587_FED_20170423a_3T2'
        '593_FED_20170423c_3T2_PL2017'
        }}); % subject IDs

exp1_spmfiles={};
for nsub=1:length(experiments(1).data),
    exp1_spmfiles{nsub}=fullfile(experiments(1).pwd1,experiments(1).data{nsub},experiments(1).pwd2,'SPM.mat');
end

exp2_spmfiles={};
for nsub=1:length(experiments(2).data),
    exp2_spmfiles{nsub}=fullfile(experiments(2).pwd1,experiments(2).data{nsub},experiments(2).pwd2,'SPM.mat');
end

ss=struct(...
    'swd','/mindhive/evlab/u/meilinz/BiLing/langfROIs_voxcount_LH_L1Eng_biling',...   % output directory
    'Localizer_spm',{cat(1,exp1_spmfiles,exp2_spmfiles)},...
    'Localizer_contrasts',{{'CHW-CHNW','ENGW-ENGNW'}},...               % localizer contrasts (overlap will be computed across these contrasts)
    'Localizer_thr_type',{{'percentile-ROI-level','none'}},...
    'Localizer_thr_p',[.1,.001],... 	  
    'ExplicitMasking',[],...   
	'overwrite',true,...                             
    'ManualROIs','/users/evelina9/fMRI_PROJECTS/ROIS/LangParcels_n220_LH.img',...
    'ask','missing');
    
if isempty(which('spm')), addpath /software/spm12; end
if isempty(which('spm_ss')), addpath /software/spm_ss; end
addpath /software/spm12; 
addpath /software/spm_ss; 

ss=spm_ss_overlap(ss);                                      % see help spm_ss_overlap for additional information





