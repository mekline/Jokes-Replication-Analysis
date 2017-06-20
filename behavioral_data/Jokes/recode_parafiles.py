# This file takes all the raw data and makes a few (many) more nicely 
# formatted files for doing analysis on the Jokes project.  See README.txt 
#for high level details!


# Open all files and make a DF for each kind: 

# - participant summary
# - parameter unfolding
# - all the data: materials and run files (which need fixing)

import glob
import pandas as pd

df_psum = pd.read_csv('/Users/mekline/Dropbox/_Projects/Jokes \
- fMRI/Jokes-Replication-Analysis/participant_summary.csv')

df_param = pd.read_csv('parameter unfolding.csv')

mat_names = glob.glob('*_materials.csv')

mat_df_list = []
for fi in mat_names:
    frame = pd.read_csv(fi, header=None)
    frame['filename'] = fi
    mat_df_list.append(frame)

df_allmats = pd.concat(mat_df_list)


run_names = glob.glob('*_data.csv')
run_df_list = []
for fi in run_names:
    frame = pd.read_csv(fi)
    frame['filename'] = fi
    run_df_list.append(frame)

df_allruns = pd.concat(run_df_list)

#NOTE: the run_data.csv files are are in a weird format, and
#need to be re-constituted to be more normal. Do that here. 

df_responses = df_allruns[df_allruns['category'].isin(['joke', 'nonjoke'])]
df_timings = df_allruns[df_allruns['category'].isin(['joke', 'nonjoke']) == 0]
df_timings = df_timings.drop(df_timings.columns[[6, 7, 8, 9, 10, 11]], axis=1)
df_timings.columns = ['onset', 'length', 'runno', 'item', 'listno', 'category', 'filename']
df_timings = df_timings[df_timings['runno'] != 'fix']#fixation is implicit!

# Merge! XXX IT DEOSN"T WORK YET. START HERE
print df_timings['item'].astype(float)
print '#############################'

print df_timings.dtypes
print df_responses.dtypes

df_rundata = pd.merge(df_timings, df_responses, on=['filename', 'item'])

print df_rundata.head(1000)

# Try to match up each person (from run summary) to all their
# corresponding files printing out a list of any subjects who
# don't get matched nicely. For simplicity of thinking about
# this, I'll store these in an array of DFs, one merged df
# per run per participant.

df_runs_per_participant = []

for participant in df_psum['Full_SessionID']:
    this_row = df_psum[df_psum['Full_SessionID'] == participant]

    this_run_params = df_param[df_param['ID'] == participant]
    # should give 1 row per existing run from the datasheets
    beh_code = this_row['Jokes_behavioral_code'].tolist()[0]
    expected_mats = beh_code + '_materials.csv'
    this_materials = df_allmats[df_allmats['filename'] == expected_mats]

    this_all_runs = df_allruns[df_allruns['filename'].str.contains(beh_code)]


# Try to match up people with datafiles
# - Get the name of the behav file from participant summary, and check that it has the right # of runs
# - If not, stop and say what's up; this is MK's chance to move extraneous csvs to Unused_files

# Try to match up files with recorded parameters
# Do this by saving a dict of the timing of the first 5 trials, which is enough to 
# diagnose optseq/counterbalancing, which is the most important thing for doing analysses right

# Assuming everything above looks copacetic, make new parafiles. Do as follows:
# For each (good) csv, just read the lines and timings, assigning conditions as we go. Keep a running
# buffer of item number, and check each one against it. If found, print the item number and break (at first, to 
# double check what's going on) or assign it the OTHER condition. Remeber that fixation is modeled implicitly!


