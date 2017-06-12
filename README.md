# Jokes-Replication-Analysis

This is the repo that contains all analysis of the 'Jokes' replication reported in **PAPER TITLE**, which is a preregistered replication of this experiment (link to repo and preprint.) Read the preprint for details, but essentially this experiment is identical except that we use a sample size calculated from the previous round of results (see LINK for these calculations), and add an additional, exploratory Theory-of-Mind localizer designed to test how/whether joke-specific activation in ToM regions is dependent on the particular localizer task used. 

## Repository Contents

* participant_summary.csv - contains basic info one each participant, including basic demographics (age, gender, handedness, langauge background), any oddities about the critical testing session, and information on localizers, including whether localizer data was collected for that participant on another day, & any exclusions from analyses & reasons for doing so.  All participants who participated in testing sessions for the Jokes study are included in this doc.

## (More interesting folders etc. up here.)

### Folder: spcorr_outputs/

Contains the scripts and results that generate correlations between contrasts across an entire parcel for an individual with multiple runs of the same task. This is used to evaluate data quality. Other scripts will read from this folder to produce more human-readable output, tests reported in the paper, etc. 

Naming convention: spcorr_TASK_Comment_Experiment_Date.m --> spcorr_results_TASK_Experiment_Date/

e.g. spcorr_LanglocSN_prevsess_Jokes2_20170523.m

('prevsess' refers to the participants whose localizer comes from another day, the other is the participants who localized during the same session as the critical experiment.)


## Folder meansignal_outputs/

Contains the scripts & results for standard fROI calculations (mean signal per contrast/per ssfROI), which other scripts read to produce human-readable output. Naming convention: 

LOCALIZERfROI_resp_TASK_comment_date.m --> LOCALIZERfROI_resp_TASK_comment_date_results/

e.g. ToMfROI_resp_Jokes_preliminary_201706012.m

The very first part of the pathway for the mean-signal analyses  (format_spsmss_results.R) also stores tidy-fied csvs of the results alongside. 


## What isn't here, and why

Analyses and data not in this repo include everything between/including raw dicom files & through the scripts that produce the wholebrain contrast files we eventually use for all 2nd-level analyses.  All of that is collected/analyzed in a very standard way in Evlab, with no changes specific to the Jokes project. The main relevant piece of info is that all analyses for this project use 'the old pipeline' (spm_ss pre- April 2017).  

Data that is openly available in this repository: 

* All behavioral responses from the experimental localizers/critical tasks (TO ADD)

*IF SPACE ALLOWS: The full wholebrain contrast maps for the crit task + all localizers, plus the parcels (ASK EV)

* Mean average signal values for all contrasts collected in the experimental localizers/critical tasks, per indivudally-calculated fROI, per person, per task/condition. These are all accompanied by the scripts that produced them from the underlying contrast data. 

*spcorr outputs (TO ADD)

* Wholebrain peak/cluster analyses (TO ADD?)

The analyses conducted in this repo can all be reproduced from this set of data (& were conducted on my laptop, not mindhive)
