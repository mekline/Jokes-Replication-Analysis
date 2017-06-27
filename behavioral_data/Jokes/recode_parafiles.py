# This file takes all the raw data and makes a few (many) more nicely 
# formatted files for doing analysis on the Jokes project.  See README.txt 
# for high level details!


# Open all files and make a DF for each kind: 

# - participant summary
# - parameter unfolding
# - all the data: materials and run files (which need fixing)

import glob
import pandas as pd
import numpy as np

df_psum = pd.read_csv('/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/participant_summary.csv')

df_param = pd.read_csv('parameter unfolding.csv')

run_names = glob.glob('Outputs_used/*_data.csv')
run_df_list = []
for fi in run_names:
    frame = pd.read_csv(fi)
    frame['filename'] = fi
    run_df_list.append(frame)

df_allruns = pd.concat(run_df_list)

#NOTE: the XXX_run_data.csv files are are in a weird format, and
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
# per participant, and concatenate at the end

df_runs_per_participant = []

for participant in df_psum['Full_SessionID']:

    this_row = df_psum[df_psum['Full_SessionID'] == participant]

    try:
        beh_code = this_row['Jokes_behavioral_code'].tolist()[0]
    except IndexError: #found empty line
            continue

    run_index_for_para = this_row['Jokes run no'].tolist()[0]

    this_run_params = df_param[df_param['ID'] == participant]
    # should give 1 row per existing run from the datasheets

    this_rundata = df_rundata[df_rundata['filename'].str.contains(beh_code)]
    this_rundata['ID'] = participant
    nRuns = len(this_rundata['filename'].unique())
    # should give set of runs with that code

    # Some column renaming etc. to facilitate merge!
    this_run_params = this_run_params.drop(['Session ID', 'Unnamed: 7'], axis=1)
    this_rundata = this_rundata.drop(['subj', 'sentence', 'joke ending', 'nonjoke ending'], axis=1)

    this_full_runddata = pd.merge(this_rundata, this_run_params, on = ['ID', 'run'], how='outer')
    
    #drop NA lines (using the sentence-displayed field, not response or anything else!)
    this_full_runddata['displayed'] = this_full_runddata['displayed'].astype(str)
    this_full_runddata = this_full_runddata[this_full_runddata['displayed'] != 'nan']

    # Time to check the structure of the runndata for goodness and then make some para files
    # (We retain the 1st 2 subjects, who saw the whole run minus just a few final trial thanks to early ending script)
    
    print ''
    if (len(this_full_runddata) == 156) & (nRuns == 3):
        print participant + 'files merged well'
    elif (len(this_full_runddata) == 104) & (nRuns == 2):
        print participant + 'files merged well'
    else:
        print participant + 'check data files'
        print nRuns
        print len(this_full_runddata)

    # Conduct some basic checks to make sure that merge and datafiles 
    # have appeared as we expect (many columns were not labeled & meaning
    # inferred from sample files!) (we can drop this later)
    # NOTE: in fact, we determined that the list is set on run 1 of a participant, 
    # and fixed thereafter. So, list_x (from this_runddata) is correct, while 
    # list_y is sometimes inaacurate bc the parameter entered doesn't match what
    # the script actually gives you. 

    print beh_code



    this_full_runddata['listno'] = this_full_runddata['listno'].astype(float)
    this_full_runddata['list_x'] = this_full_runddata['list_x'].astype(float)
    this_full_runddata['list_y'] = this_full_runddata['list_y'].astype(float)

    this_full_runddata['Run number'] = this_full_runddata['Run number'].astype(float)
    this_full_runddata['run'] = this_full_runddata['run'].astype(float)
    this_full_runddata['runno'] = this_full_runddata['runno'].astype(float)

    this_full_runddata['listcheck'] = (this_full_runddata['listno'] == 
        this_full_runddata['list_x'])

    this_full_runddata['runcheck'] = (this_full_runddata['runno'] == 
        this_full_runddata['run'])  & (
        this_full_runddata['run'] ==
        this_full_runddata['Run number'])

    # All data is validated! Format and print out a few things
    # - Giant well-formatted longform version of the behavioral data (save to the big dataframe)
    # - Individualized para files for each participant - joke/nonjoke and custom
    # - Cat file for each participant

    df_runs_per_participant.append(this_full_runddata)

    cat_file_name = 'Cat_files/' + participant + '_Jokes.cat'
    cat_file_name_custom = 'Cat_files/' + participant + '_Jokes_Custom.cat'
    para_filenames_for_cat = []
    para_filenames_for_cat_custom = []

    #Make each para file
    print this_full_runddata['run'].unique()

    for i in this_full_runddata['run'].unique():

    	#Filenames
    	thispara = 'nonlit_joke_' + participant + '_run' + str(int(i)) + '.para'
    	para_filenames_for_cat.append('nonlit_joke_individual_paras/' + thispara)
        thispara_out = 'Para_indiv_jnj/'+thispara

        print thispara
        
        thispara_custom = 'nonlit_joke_' + participant + '_run' + str(int(i)) + '_customratings.para'
        para_filenames_for_cat_custom.append('nonlit_joke_individual_paras_custom/' + thispara_custom)
        thispara_custom_out = 'Para_indiv_custom/'+thispara_custom

        #get the relevant lines
        this_run_onsets = this_full_runddata[this_full_runddata['run'] == i]
        this_run_onsets['tr'] = this_run_onsets['onset'].astype(float) / 2
        this_run_onsets['catno'] = np.where(this_run_onsets['category']=='joke', '1', '2')

        #recode ratings! Note, if they don't give a response, we code as 1/other
        this_run_onsets['response'] = np.where(this_run_onsets['response'].isnull(), 0,this_run_onsets['response'])

        this_run_onsets['rating_levels'] = this_run_onsets['response'].astype(int) + 1

        this_run_onsets['rating_levels'] = np.where((this_run_onsets['rating_levels'] == 5), 4, this_run_onsets['rating_levels'])
        
        onsetstrings_jnj = []
        onsetstrings_custom = []
        for index, line in this_run_onsets.iterrows():

        	mystr = str(int(line['tr'])) + ' ' + str(line['catno'])
        	onsetstrings_jnj.append(mystr)

        	mystr_custom = str(int(line['tr'])) + ' ' + str(line['rating_levels'])
        	onsetstrings_custom.append(mystr_custom)

       	parastring = '#onsets\n' + '\n'.join(onsetstrings_jnj) + '\n\n#names\njoke lit\n\n#durations\n4 4'
       	with open(thispara_out, "w") as text_file:
       		text_file.write(parastring)

       	parastring_custom = '#onsets\n' + '\n'.join(onsetstrings_custom) + '\n\n#names\nother low med high\n\n#durations\n4 4 4 4'
       	with open(thispara_custom_out, "w") as text_file:
       		text_file.write(parastring_custom)
       	
    #After making each para file, make the cat file
    catstring = '#runs ' + run_index_for_para + '\n#path /mindhive/evlab/u/Shared/PARAS/\n#files\n' + '\n'.join(para_filenames_for_cat)
    with open(cat_file_name, "w") as text_file:
    	text_file.write(catstring)

   	catstring_custom = '#runs ' + run_index_for_para + '\n#path /mindhive/evlab/u/Shared/PARAS/\n#files\n' + '\n'.join(para_filenames_for_cat_custom)
    with open(cat_file_name_custom, "w") as text_file:
    	text_file.write(catstring_custom)
    	
# End participant loop
# and print out the big datafile!
alldata = pd.concat(df_runs_per_participant)
alldata = alldata[['ID', 'filename', 'listno', 'runno', 'onset', 'length', 'item',  'category', 'displayed', 'response', 'RT', 'question onset',  'para', 'counter', 'listcheck', 'runcheck']]
alldata.to_csv('all_behavioral_output.csv', index = False, na_rep='NA')

