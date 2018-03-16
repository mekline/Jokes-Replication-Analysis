# Pipeline README

This folder contains all code necessary to get the results and figures presented in Kline et al (PREPRINT) from (1) behavioral data collected during the study and (2) the output of all second-level analyses (which are lab-standardized and unmodified for this experiment, though see note on transition from old to new PL2017 preprocessing pipeline.)  This includes data from *both* experiments, though note that data from E1 comes pre-tidied (can be reproduced from separate repo - [https://github.com/mekline/Jokes-Analysis](https://github.com/mekline/Jokes-Analysis) - if you really want.)

The analyses conducted in this repo can all be reproduced from this raw data (& were conducted on my laptop, not mindhive), but for sanity we also provide the Rdataframe with NAMED ROIs and conditions! 

Numbering of R scripts = recommended order to run them in. 


`0_load_and_code_jokes.R` Optionally run this to reproduce the allSigChange.RData file; note that this includes only the top-10-percent voxel selection method versions (To produce the top-50-voxel model, change line 31 and rename the output file something different.)

`1_do_all_t_tests.R` and `2_do_all_lmers.R` run all the individual-ROI tests reported in the paper (woo!) The t-test script prints out a full table, since we report these as tables in the supplemental (`jokes_t_tests_all.csv` is the raw output, `jokes_t_tests_annotated.csv` is manually marked up for pretty tables.)  The LMER file similarly produces `jokes_LMER_confirmatory.csv`. The LMER file also contains simple effect size calculations (just mean signal change at the system level for critical analyses) that are most relevant back in E1 but included here for completion. 

TODO: Harmonize these with the new naming scheme necessary for Cloudy values

`3_system_effect_sizes.R` calculates mean signal changes to the Jokes task in each functionally defined system

`4_behavioral_localizer correlations.R` Calculates just that! In Experiment 1, a significant correlation was observed between how funny you found the jokes and your joke-lit activation in ToM regions; this was not replicated in E2.

`5_composite_graphs.R` Generates the confirmatory graphs, but you probably want to just look in the figs folder.  It generates the `mystats.RData` object for convenience of graphing from descriptive statistics. 

`6_exploratory_ROISizeCompare.R` A standalone script that performs a nonparametric analysis to see if the sizes of patches within ROI systems are sigificantly different in size from one another (Upshot: language regions are smaller, but the average sizes of ROIs in the MD and ToM masks are similar to each other.) 

