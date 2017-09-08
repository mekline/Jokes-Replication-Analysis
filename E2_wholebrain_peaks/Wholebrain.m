% second level (RFX) analysis

rerun_firstlevel_contrasts      = 0*[0];
rerun_secondlevel_analyses      = 1*[1];

%% define experiment structures
experiments(1)=struct(...
    'name','Jokes',...% non-language expt
    'pwd1','/mindhive/evlab/u/Shared/SUBJECTS/',...
    'pwd2','firstlevel_Jokes',...
    'data',{{'168_FED_20161228b_3T2',...
                '288_FED_20170412b_3T2',...
                '290_FED_20170426a_3T2',...
                '301_FED_20161217b_3T2',...
                '334_FED_20161221a_3T2',...
                '343_FED_20161208a_3T2',...
                '366_FED_20161205a_3T2',...
                '424_FED_20161228c_3T2',...
                '426_FED_20161215c_3T2',...
                '430_FED_20170426d_3T2',...
                '473_FED_20170210b_3T2',...
                '498_FED_20170210c_3T2',...
                '520_FED_20161227a_3T2',...
                '521_FED_20161228a_3T2',...
                '551_FED_20170412a_3T2',...
                '555_FED_20170426c_3T2',...
                '571_FED_20170412c_3T2',...
                '576_FED_20170414b_3T2',...
                '577_FED_20170414c_3T2',...
                '578_FED_20170414d_3T2',...
                '596_FED_20170426b_3T2'}});

%% first-level contrasts
existingcontrasts=[];
if any(rerun_firstlevel_contrasts),
    addpath('/mindhive/evlab/u/Shared/ANALYSIS');
    for exp=find(rerun_firstlevel_contrasts),
        for nsub=1:length(experiments(exp).data),
            subjectname=experiments(exp).data{nsub};
            foldername=fullfile(experiments(exp).pwd1,subjectname,experiments(exp).pwd2);
            load(fullfile(foldername,'SPM.mat'));
            SPM.swd = foldername;
            save(fullfile(foldername,'SPM.mat'),'SPM');
            jobs{1}.stats{1}.con.spmmat = {fullfile(foldername,'SPM.mat')};
            jobs{1}.stats{1}.con.consess = feval(['build_contrasts_',experiments(exp).name],subjectname,fullfile(foldername,'SPM.mat'));
            spm_jobman('run',jobs);
        end
    end
end


%% second-level analyses
if any(rerun_secondlevel_analyses),
    cwd=pwd;
    spm('Defaults','fmri');
    addpath('/mindhive/evlab/u/Shared/ANALYSIS');
    for exp=find(rerun_secondlevel_analyses),
        contrasts = eval(['build_contrasts_',experiments(exp).name]);
        foldername1=[experiments(exp).pwd1];
        ok=mkdir(foldername1,['secondlevel_',experiments(exp).name]);
        foldername1=fullfile(foldername1,['secondlevel_',experiments(exp).name]); %e.g. /groups/domspec/data/secondlevel_SWJN/
        for ncon=1:length(contrasts),
            if ~any(existingcontrasts==ncon),
                ok=mkdir(foldername1,contrasts{ncon}{1});
                foldername3=fullfile(foldername1,contrasts{ncon}{1});                   %e.g. /groups/domspec/data/secondlevel_SWJN/S-J/
                
                cd(foldername3);
                clear SPM;
                SPM.xX.X=ones(length(experiments(exp).data),1);
                SPM.xX.name={[contrasts{ncon}{1},' ',experiments(exp).name]};
                for nsub=1:length(experiments(exp).data),
                    filename=fullfile(experiments(exp).pwd1,experiments(exp).data{nsub},experiments(exp).pwd2,['con_',num2str(ncon,'%04d'),'.img']);
                    SPM.xY.VY(nsub,1)=spm_vol(filename);
                end
                save('SPM.mat','SPM');
                !rm SPM.mat
                !rm mask.img
                SPM=spm_spm(SPM);
                c=1;
                cname=SPM.xX.name{1};
                SPM.xCon = spm_FcUtil('Set',cname,'T','c',c',SPM.xX.xKXs);
                SPM=spm_contrasts(SPM,1:length(SPM.xCon));
                save('SPM.mat','SPM');
                cd(cwd);
            end
        end
    end
end
