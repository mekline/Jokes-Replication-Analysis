# Jokes-Replication-Analysis

This is the repo that contains all analysis of the 'Jokes' replication reported in **PAPER TITLE**, which is a preregistered replication of this experiment (link to repo and preprint.) Read the preprint for details, but essentially this experiment is identical except that we use a sample size based on power analysis from the previous round of results (see LINK for these calculations), and add an additional, exploratory Theory-of-Mind localizer designed to test how/whether joke-specific activation in ToM regions is dependent on the particular localizer task used. 


## Repository Contents

`participant_summary.csv` - contains basic info one each participant, including basic demographics (age, gender, handedness, langauge background), any oddities about the critical testing session, and information on localizers, including whether localizer data was collected for that participant on another day, & any exclusions from analyses & reasons for doing so.  All participants who participated in testing sessions for the Jokes study are included in this doc.

`Testing Records.xlsx` - Locked; primarily a process document for tracking analyses, locked bc contains some PPI. Contact mekline@mit.edu if you really need this.

### Folder: Analysis pipeline

Running the scripts in this folder in order should produce all statistics and figures reported in the paper. (And definitely do so, on Melissa's laptop.) 

### Folder E1_tabular_data_outputs/

Contains both behavioral & neural data (just the well-formatted CSVs) from the original version of this study; full documentation of that project is available at [https://github.com/mekline/Jokes-Analysis](https://github.com/mekline/Jokes-Analysis), but all reported valued from that study are calculated in *this* pipeline to ensure everything is parallel. 

### Folder E2_meansignal_outputs/

Contains the scripts & results of the standardized fROI calculations (mean signal per contrast/per ssfROI/per person), see README in that folder.

### Folder: E2_behavioral_data/
Raw behavioral data produced by participants during the critical Jokes task

### Folder: E2_spcorr_outputs/

Contains the scripts and results that generate correlations between contrasts across an entire parcel for an individual with multiple runs of the same task. Naming convention: spcorr_TASK_Comment_Experiment_Date.m --> spcorr_results_TASK_Experiment_Date/

e.g. `spcorr_LanglocSN_prevsess_Jokes2_20170523.m`

('prevsess' refers to the participants whose localizer comes from another day, the other is the participants who localized during the same session as the critical experiment.)

### Folder: E2_voxel_overlap/

Voxel overlap analyses from the 2 ToM tasks (langauge based and Cloudy) in the corresponding ROIs.

### Folder: E2_wholebrain_peaks/

Script & output of standard whole-brain peak analyses, done in SPM/Matlab


## What isn't here, and why

Analyses and data not in this repo include everything between/including raw dicom files & through the scripts that produce the wholebrain contrast files we eventually use for all 2nd-level analyses.  All of that is collected/analyzed in a very standard way in Evlab, with no changes specific to the Jokes project. The main relevant piece of info is that all analyses for this project use 'the old pipeline' (spm_ss pre- April 2017), EXCEPT for those involving the Cloudy task, which breaks the localizer by having events of varying duration.   

Data that is openly available in this repository: 

*All behavioral responses from the experimental localizers/critical tasks (XXXTO ADD Localizer Data!)*

*IF SPACE ALLOWS: The full wholebrain contrast maps for the crit task + all localizers, plus the parcels (ASK EV)*

* Mean average signal values for all contrasts collected in the experimental localizers/critical tasks, per indivudally-calculated fROI, per person, per task/condition. These are all accompanied by the scripts that produced them from the underlying contrast data. 


