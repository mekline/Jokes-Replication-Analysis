# E2_meansignal_outputs README

This folder contains all the finalized data used to run ROI-based mean-signal analyses replorted the Jokes project (including both preregistered and exploratory analyses.) 

## Most important files

The most important file is `all_mean_signal_outputs.csv`, which collates the by-subject, by-condition, and by-fROI mean signal activation results into a single tabular format. This is the rawest tabular form of the data, but recommend you use the RData generated over in the analysis pipeline (and copied here for convenience as it adds significantly more human-readable information!

`all_mean_signal_outputs_CODEBOOK.csv` explains the columns in that file. 

The `all_mean_signal_outputs.csv` document can be re-created from the raw spmss outputs by running `!make_all_mean_signal_outputs.R`

If necessary, use `convert_spmmss_results.R` to format an individual analysis set into tidy format. (Need to do this for PL2017, as the maker uses the csv instead of digging into the original files directly.)

## Toolbox output files

The majority of rest of the files in this folder are triples of a subfolder, matlab script, and CSV with the same name, e.g. LangfROIs_resp_Jokes_20170703. The matlab script, when run from mindhive with access to the preprocessed nifti-format brain data, produces the output folder. We then use `convert_smpss_results.R` to produce the corresponding CSV file. The naming scheme used is as follows:

`LocalizerfROIs_resp_CriticalTask_Date`

*LocalizerfROIs* gives the mask & task used to restrict search spaces for individual fROIs; only the system of interest is listed; the particular versions used are just the evlab standard as of 2017. Unless otherwise stated (e.g. CloudyToMfROIs), the task is the standard evlab localizer task. 

*CriticalTask* is the source of activation values measured in the fROIs - it can be the same (automatically orthogonalized) or a different task. 

*Date* is the date the analysis was written & run.

Unless otherwise specified in the filename, individual fROIs are identified using the top 10% of voxels per ROI, and the pipeline is the *old* standard pipeline (vs. PL2017, used to analyze anything involving the Cloudy scan, which breaks the old pipeline.)

## preliminary analyses/

This folder also contains *all* analyses ever run for the project (barring those known to be actual garbage); these are mostly analyses that were formatted differently & replaced with more standardized code across analyses; some were conducted before all data collection was complete, but no changes to the data collection/preregistration plan were made based on these analyses. 