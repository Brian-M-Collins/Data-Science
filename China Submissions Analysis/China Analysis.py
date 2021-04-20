# %%
import numpy as np
import pandas as pd
# %%
file_name =  'FILE PATH'
subs_file_sheet =  'Submissions'
output_file_sheet = 'Output'
# %%
#importing file into pandas DF, dropping unncessary columns and setting journal title to row index
df_subs = pd.read_excel(io=file_name, sheet_name=subs_file_sheet)
df_pub = pd.read_excel(io=file_name, sheet_name=output_file_sheet)
df_subs.drop(['Unnamed: 9','Unnamed: 10','Unnamed: 11','Unnamed: 12'], axis=1, inplace=True)
df_subs.set_index('Journal Titles', inplace=True)
df_pub.set_index('Journal Titles', inplace=True)
# %%
#creating a new dataframe with 'Total' columns for both submissions and published out, 
#then establishing rejected articles and rejected as a percentage of submitted, sorting values by final column in descending order
df_total = df_subs['Total']
df_total = pd.DataFrame(df_total)
df_total.rename(columns = {'Total':'Submissions'}, inplace = True)
df_total['Published'] = df_pub['Total']
df_total['Rejected'] = df_total['Submissions']-df_total['Published']
df_total['Rejected %age'] = df_total['Rejected']/df_total['Submissions']
df_total.sort_values(by=['Rejected %age'], ascending=False, inplace=True)
df_total.describe()
# %%
#identifying the index and deleting the  index of titles that have a rejected article percentage of 50% or fewer. Saved as a new dataframe
index_names_percent = df_total[df_total['Rejected %age'] < 0.5 ].index
df_target_list = df_total.drop(index_names_percent, axis=0)
# %%
#identifying the index of and deleting the row of any title that has a submission value outside of the 75th percentile or above.
index_names_subs = df_total[df_total['Submissions'] < 170 ].index
df_target_list = df_total.drop(index_names_subs, axis=0)
# %%
#saving file locally
df_target_list.to_excel(r'FILE PATH', index=True)
