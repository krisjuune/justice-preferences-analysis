import pingoin as pg

# %% ################### check ICC for justice ##########################
#TODO move to a separate script later
lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian', 
                       'justice_general_1', 
                       'justice_tax_1', 
                       'justice_subsidy_1',
                       'justice_general_2', 
                       'justice_tax_2',
                       'justice_subsidy_2',
                       'justice_general_3',
                       'justice_tax_3',
                       'justice_subsidy_3',
                       'justice_general_4',
                       'justice_tax_4',
                       'justice_subsidy_4',
                       'speeder', 
                       'laggard', 
                       'inattentive']]


lpa_data = lpa_data[(df_justice['speeder'] == False) & 
              (df_justice['laggard'] == False) & 
              (df_justice['inattentive'] == False)]

df_long = lpa_data.melt(id_vars=['ID'], 
                        value_vars=['justice_general_1', 'justice_tax_1', 'justice_subsidy_1',
                                    'justice_general_2', 'justice_tax_2', 'justice_subsidy_2',
                                    'justice_general_3', 'justice_tax_3', 'justice_subsidy_3',
                                    'justice_general_4', 'justice_tax_4', 'justice_subsidy_4'],
                        var_name='variable', value_name='score')

# Extract set number from variable name (1, 2, 3, or 4)
df_long['principle'] = df_long['variable'].str.extract(r'(\d+)')

icc_results = pg.intraclass_corr(data=df_long, 
                                 targets='ID', 
                                 raters='principle', 
                                 ratings='score')

print(icc_results)

# p-values are actually so small they get shown as 0.0, 
# tested by only looking at general and subsidy