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

df_timings['item'] = df_timings['item'].astype(float)
df_timings['category'] = df_timings['category'].astype(str)
df_timings['filename'] = df_timings['filename'].astype(str)

df_responses['item'] = df_responses['item'].astype(float)
df_responses['category'] = df_responses['category'].astype(str)
df_responses['filename'] = df_responses['filename'].astype(str)

df_rundata = pd.merge(df_timings, df_responses, on=['filename', 'item', 'category'], how='outer')

# Try to match up each person (from run summary) to all their
# corresponding files, printing out a list of any subjects who
# don't get matched nicely. For simplicity of thinking about
# this, I'll store these in an array of DFs, one merged df
# per run per participant.

df_runs_per_participant = []

for participant in df_psum['Full_SessionID']:

    this_row = df_psum[df_psum['Full_SessionID'] == participant]

    try:
        beh_code = this_row['Jokes_behavioral_code'].tolist()[0]
    except IndexError:
            print 'empty line'
            print len(this_row)
            continue
    # (that's all we need from the participant summary df anymore!)

    this_run_params = df_param[df_param['ID'] == participant]
    # should give 1 row per existing run from the datasheets

    expected_mats = beh_code + '_materials.csv'
    this_materials = df_allmats[df_allmats['filename'] == expected_mats]
    this_materials['ID'] = participant
    # (we are just looking at the materials files to understand structure, will use 'empirical' timings/items for para files)

    this_rundata = df_rundata[df_rundata['filename'].str.contains(beh_code)]
    this_rundata['ID'] = participant
    nRuns = len(this_rundata['filename'].unique())

    # Some column renaming etc. to facilitate merge!
    this_run_params = this_run_params.drop(['Session ID', 'Unnamed: 7'], axis=1)
    this_materials = this_materials.drop(this_materials.columns[[4, 5, 6]], axis=1)
    this_materials.columns = ['mysteryno', 'item', 'list-maybe', 'category', 'displayed','filename', 'ID']
    this_rundata = this_rundata.drop(['subj', 'sentence', 'joke ending', 'nonjoke ending'], axis=1)

    this_full_runddata = pd.merge(this_rundata, this_run_params, on = ['ID', 'run'], how='outer')
    
    # Time to check the structure of the runndata for goodness and then make some para files
    # For NOW (until extra runs have been ID'd and placed in 'extra runs/ folder')
    # we just skip & note any subject who we don't manage to find well-formed data for

    if (len(this_full_runddata) == 156) & (nRuns == 3):
        print participant + ' files merged well'
    elif (len(this_full_runddata) == 104) & (nRuns == 2):
        print participant + ' files merged well'
    else:
        print participant
        print 'check data files!'
        continue

    # Conduct some basic checks to make sure that merge and datafiles 
    # have appeared as we expect (many columns were not labeled & meaning
    # inferred from sample files!) (we can drop this later)

    this_full_runddata['displayed'] = this_full_runddata['displayed'].astype(str)
    this_full_runddata = this_full_runddata[this_full_runddata['displayed'] != 'nan']
    #drop empty lines

    this_full_runddata['listno'] = this_full_runddata['listno'].astype(float)
    this_full_runddata['list_x'] = this_full_runddata['list_x'].astype(float)
    this_full_runddata['list_y'] = this_full_runddata['list_y'].astype(float)

    this_full_runddata['Run number'] = this_full_runddata['Run number'].astype(float)
    this_full_runddata['run'] = this_full_runddata['run'].astype(float)
    this_full_runddata['runno'] = this_full_runddata['runno'].astype(float)

    this_full_runddata['listcheck'] = (this_full_runddata['listno'] == 
        this_full_runddata['list_x'])  & (
        this_full_runddata['list_x'] ==
        this_full_runddata['list_y'])

    this_full_runddata['runcheck'] = (this_full_runddata['runno'] == 
        this_full_runddata['run'])  & (
        this_full_runddata['run'] ==
        this_full_runddata['Run number'])

    #print this_full_runddata[['listcheck', 'list_x']].to_string() #these are the 'empirical' ones recorded in the thing
    #print this_full_runddata[['runcheck', 'runno']].to_string()

    # Fascinating hypothesis! Maybe the script used the same materials once the subj code existed? the list no is always matching whatever was for run 1
    # Let's check whether there are doubles on the run numbers!


    print len(this_full_runddata['item'])
    print len(this_full_runddata['item'].unique())




        

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


