import pandas as pd
import numpy as np
from scipy.stats import norm

def prep_conjoint(df, 
                  respondent_columns=['responseId', 'gender', 'age'], 
                  regex_list='pv|mix|imports|tradeoffs|distribution', 
                  filemarker='stack-choice', 
                  calculate_ratings=True):

    '''
    Change the conjoint data from wide to long format. 

    Parameters: 
    - df: pandas dataframe from Qualtrics with column names including 
    'choice' for discrete choice evaluations and column names including 
    'rating' for rated evaluations of the conjoint
    - respondent_columns: by default three basic columns as chosen, 
    otherwise provide a vector of strings, containing the desired 
    column names 
    '''

    # select data columns per experiment
    df_task = df.filter(regex="ID|^choice(?!$)")
    df_task = df_task.drop(columns=df_task.filter(regex=regex_list).columns)

    # drop data rows per experiment 
    df_task = df_task.dropna() # this doesn't work for pv because the pv table is saved for all, the NaN values are in the choice and rating columns
    
    # reshape the attributes for both experiments, so each combination (?) gets its own row
    df_task_melted = df_task.melt(id_vars='ID', var_name='variable', value_name='value')

    # add task, package choice, and attribute numbering
    df_task_melted['task_num'] = df_task_melted['variable'].str.extract(r'(\d+)').astype(int)
    df_task_melted['pack_num'] = df_task_melted['variable'].str.extract(r'(\d)$').astype(int)
    df_task_melted['attribute'] = df_task_melted['variable'].str.extract(r'_(.*)$')
    df_task_melted['pack_num_cat'] = df_task_melted['pack_num'].astype(str).map({'1': 'Left', '2': 'Right'})

    # pivot to wide format
    df_task_pivoted = df_task_melted.pivot_table(index=['ID', 
                                                        'task_num', 
                                                        'pack_num_cat', 
                                                        'pack_num'], 
                                                columns='attribute', 
                                                values='value', 
                                                aggfunc='first').reset_index() # aggfunc first to pick the first value in a group, there were no duplicates anyway but the default expects numeric data

    # create task 8 data
    task1_data = df_task_pivoted[df_task_pivoted['task_num'] == '1']
    task8 = task1_data.copy()
    task8['task_num'] = '8'
    task8['pack_num'] = task8['pack_num'].replace({'1': '2', '2': '1'})
    task8['pack_num_cat'] = task8['pack_num_cat'].replace({'Left': 'Right', 'Right': 'Left'})

    # merge pivoted df with task 8 data 
    df_task_merged = pd.concat([df_task_pivoted, task8], ignore_index=True)
    df_task_merged = df_task_merged.sort_values(by=['ID', 'task_num'])

    # reshape the respondents' preferences so each choice gets its own row
    df_choice = df.drop(columns=df.filter(regex=regex_list).columns)
    df_choice = df_choice.filter(regex='ID|choice$').dropna() # filter only choice columns and drop the other experiment's participants
    df_choice_melted = df_choice.melt(id_vars='ID', var_name='variable', value_name='choice') # reshape from wide to long 
    df_choice_melted['task_num'] = df_choice_melted['variable'].str.extract(r'(\d+)').astype(int) # extract the task number
    df_choice_melted['choice'] = df_choice_melted['choice'].str.replace('Massnahmenpaket', '').astype(int) # convert choice to numeric
    df_choice = df_choice_melted.drop(columns=['variable']) # drop the 'variable' column

    # merge attributes and preferences
    stack_choice = pd.merge(df_task_merged, df_choice, on=['ID', 'task_num'], how='left')
    stack_choice['Y'] = (stack_choice['pack_num'] == stack_choice['choice']).astype(int) # Create the 'Y' column where 1 indicates that the package was chosen, 0 otherwise
    
    # merge with respondents data 
    stack_choice_res = pd.merge(stack_choice, respondent_columns, on='ID', how='left')

    # aggregate table1 and table2 columns
    table2_cols = [col for col in stack_choice_res.columns if col.endswith('_table2')]
    table1_cols = [col.replace('_table2', '_table1') for col in table2_cols]
    for table1, table2 in zip(table1_cols, table2_cols): # move non-NaN values from '_table2' columns to '_table1' columns
        stack_choice_res[table1] = stack_choice_res[table1].combine_first(stack_choice_res[table2])
    stack_choice_res.rename(columns=lambda x: x.replace('_table1', ''), inplace=True) # remove the '_table1' suffix from the column names
    stack_choice_res.drop(columns=table2_cols, inplace=True) # drop the '_table2' columns

    # check that no extra rows were created
    # output needs to be the nr of respondents taking part in the experiment times the nr of choices per task times the nr of tasks
    if len(stack_choice) == (len(df_task) * stack_choice['pack_num'].max() * stack_choice['task_num'].max()):
        print("Conjoint data preparation successful")
    else:
        raise ValueError("Error: The lengths of input df and output df do not match. Check input data.")
    
    # calculate ratings if True and save to file
    if calculate_ratings == True: 
        #TODO the rating data has the missing data problem that was true 
        # for the choices before, there some kind of error with 
        # stacking in prep data, looks like the right rating isn't
        # matched with the right choice

        # reshape the respondets' preferences so each rating gets its own row
        df_rating = df.drop(columns=df.filter(regex=regex_list).columns)
        df_rating = df_rating.filter(regex='ID|-rating_').dropna() # here I get 1062 respondents but with choice 1068 - how??
        df_rating_melted = df_rating.melt(id_vars='ID', var_name='variable', value_name='rating')
        df_rating_melted['rating'] = df_rating_melted['rating'].astype(int)
        df_rating_melted['task_num'] = df_rating_melted['variable'].str.extract(r'-rating_(\d+)').astype(int)
        df_rating_melted['pack_num'] = df_rating_melted['variable'].str.extract(r'(\d)$').astype(int)
        df_rating = df_rating_melted.drop(columns=['variable', 'pack_num'])
        
        # merge rating data
        stack_rating = pd.merge(df_task_merged, df_rating, on=['ID', 'task_num'], how='left')
        stack_rating_res = pd.merge(stack_rating, respondent_columns, on='ID', how='left')

        # aggregate table1 and table2 columns
        table2_cols = [col for col in stack_rating_res.columns if col.endswith('_table2')]
        table1_cols = [col.replace('_table2', '_table1') for col in table2_cols]
        for table1, table2 in zip(table1_cols, table2_cols): # move non-NaN values from '_table2' columns to '_table1' columns
            stack_rating_res[table1] = stack_rating_res[table1].combine_first(stack_rating_res[table2])
        stack_rating_res.rename(columns=lambda x: x.replace('_table1', ''), inplace=True) # remove the '_table1' suffix from the column names
        stack_rating_res.drop(columns=table2_cols, inplace=True) # drop the '_table2' columns

        # stack choice and rating files together
        stack_both_res = pd.merge(stack_choice_res, 
                                  stack_rating_res[['ID', 'task_num', 'pack_num', 'rating']],
                                  on=['ID', 'task_num', 'pack_num'], 
                                  how='left')

        # for pv experiment, there is still missing data, so drop all rows where choice is NaN
        stack_both_res = stack_both_res.dropna(subset=['choice'])

        # save to file
        stack_both_res.to_csv(f'data/{filemarker}-conjoint.csv', index=False)
        print(f'Stacked choice and rating data saved to file data/{filemarker}-conjoint.csv')
        return stack_both_res

    else: 
        stack_choice_res = stack_choice_res.dropna(subset=['choice'])
        stack_choice_res.to_csv(f'data/{filemarker}-choices.csv', index=False)
        print(f'Stacked choice data saved to file data/{filemarker}-choices.csv')
        return stack_choice_res
    

def calculate_IRR(df, 
                  amce, 
                  n_respondents):
    '''
    Description
    '''
    # filter choices for tasks 1 and 8
    IRR_task1_choice = df[df['task_num'] == 1][['ID', 'choice']]
    IRR_task8_choice = df[df['task_num'] == 8][['ID', 'choice']]
    
    # merge tasks 1 and 8
    IRR_tasks1_8_choice = pd.merge(IRR_task1_choice, IRR_task8_choice, on='ID', suffixes=('_task1', '_task8'))
    IRR_tasks1_8_choice.columns = ['ID', 'task_num1', 'task_num8']
    
    # 1 in both tasks
    both_chose_1 = IRR_tasks1_8_choice[(IRR_tasks1_8_choice['task_num1'] == 1) & (IRR_tasks1_8_choice['task_num8'] == 1)]
    num_both_chose_1 = len(both_chose_1)
    print(f"Number of respondents who chose 1 in both tasks: {num_both_chose_1}")

    # 2 in both tasks
    both_chose_2 = IRR_tasks1_8_choice[(IRR_tasks1_8_choice['task_num1'] == 2) & (IRR_tasks1_8_choice['task_num8'] == 2)]
    num_both_chose_2 = len(both_chose_2)
    print(f"Number of respondents who chose 2 in both tasks: {num_both_chose_2}")

    # 2 in task 1 and 1 in task 8
    b_chose = IRR_tasks1_8_choice[(IRR_tasks1_8_choice['task_num1'] == 2) & (IRR_tasks1_8_choice['task_num8'] == 1)]
    num_b_chose = len(b_chose)
    print(f"Number of respondents who chose 2 in task 1 and 1 in task 8: {num_b_chose}")

    # 1 in task 1 and 2 in task 8
    c_chose = IRR_tasks1_8_choice[(IRR_tasks1_8_choice['task_num1'] == 1) & (IRR_tasks1_8_choice['task_num8'] == 2)]
    num_c_chose = len(c_chose)
    print(f"Number of respondents who chose 1 in task 1 and 2 in task 8: {num_c_chose}")

    # calculate IRR
    IRR_choice = (num_b_chose + num_c_chose) / (num_both_chose_1 + num_b_chose + num_both_chose_2 + num_c_chose)
    print(f"IRR Choice: {IRR_choice}")

    # confidence interval for IRR
    IRR_SE_choice = np.sqrt((IRR_choice * (1 - IRR_choice)) / n_respondents)
    z_critical = norm.ppf(0.975)  # 95% confidence interval
    CI_plus = IRR_choice + (z_critical * IRR_SE_choice)
    CI_minus = IRR_choice - (z_critical * IRR_SE_choice)
    
    print(f"CI Plus: {CI_plus}")
    print(f"CI Minus: {CI_minus}")

    # swap error
    swap_error_choice = (1 - np.sqrt(1 - (2 * (1 - IRR_choice)))) / 2
    print(f"Swap Error Choice: {swap_error_choice}")

    # correct the AMCE
    amce_corrected = amce.copy()
    amce_corrected['estimate'] = (amce_corrected['estimate']) / (1 - (2 * swap_error_choice))
    amce_corrected['std.error'] = (amce_corrected['std.error']) / (1 - (2 * swap_error_choice))
    amce_corrected['z'] = amce_corrected['estimate'] / amce_corrected['std.error']
    amce_corrected['p'] = 2 * (1 - norm.cdf(np.abs(amce_corrected['z'])))
    amce_corrected['lower'] = amce_corrected['estimate'] - 1.96 * amce_corrected['std.error']
    amce_corrected['upper'] = amce_corrected['estimate'] + 1.96 * amce_corrected['std.error']

    print(amce_corrected)

    return amce_corrected