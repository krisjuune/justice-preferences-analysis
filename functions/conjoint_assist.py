import pandas as pd

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
    #TODO this still doesn't work for pv data
    choice_columns = df_task.filter(like='choice').columns
    df_task = df_task.dropna(subset=choice_columns)
    # df_task = df_task.dropna() # this doesn't work for pv because the pv table is saved for all, the NaN values are in the choice and rating columns
    
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
    df_choice = df.drop(columns=df.filter(regex='pv|Mix|Imports|Rooftop|Infrastructure|Distribution').columns)
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
        raise ValueError("Error: The lengths do not match. Check input data.")
    
    # save to file
    # TODO move saving to else, and make one stacked file if rating is True instead of one for choices and one for ratings
    

    if calculate_ratings == True: 
        # reshape the respondets' preferences so each rating gets its own row
        df_rating = df.drop(columns=df.filter(regex=regex_list).columns)
        df_rating = df_rating.filter(regex='ID|-rating_').dropna() # here I get 1062 respondents but with choice 1068 - how??
        df_rating_melted = df_rating.melt(id_vars='ID', var_name='variable', value_name='rating')
        df_rating_melted['rating'] = df_rating_melted['rating'].astype(int)
        df_rating_melted['task_num'] = df_rating_melted['variable'].str.extract(r'-rating_(\d+)').astype(int)
        df_rating_melted['pack_num'] = df_rating_melted['variable'].str.extract(r'(\d)$').astype(int)
        df_rating = df_rating_melted.drop(columns='variable')
        
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

        # smth funny happens so the rating df uses the pre existing choice one, 
        # and so it creates pack_num_x and y and that's why pack_num doesn't exist for merge

        # stack choice and rating files together
        # stack_both_res = pd.merge(stack_choice_res, 
        #                           stack_rating_res[['ID', 'task_num', 'pack_num', 'rating']],
        #                           on=['ID', 'task_num', 'pack_num'], 
        #                           how='left')

        # save to file
        # stack_both_res.to_csv(f'data/{filemarker}-conjoint.csv', index=False)
        # print(f'Stacked choice and rating data saved to file data/{filemarker}-conjoint.csv')
        return stack_rating_res

    # else: 
        # stack_choice_res.to_csv(f'data/{filemarker}-choices.csv', index=False)
        # print(f'Stacked choice data saved to file data/{filemarker}-choices.csv')
        # return stack_choice_res