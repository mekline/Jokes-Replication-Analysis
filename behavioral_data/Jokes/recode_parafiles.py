# PSEUDOCODE FOR THE PARAFILE EXTRAVAGANZA

import csv
import pandas as pd

df = pd.read_csv('/Users/mekline/Dropbox/_Projects/Jokes - fMRI/Jokes-Replication-Analysis/participant_summary.csv')

print df.column
print df.loc['KlineID'] #this doesn't work yet


# Open files
# - participant summary
# - parameter unfolding
# - all the data: materials and run files

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


