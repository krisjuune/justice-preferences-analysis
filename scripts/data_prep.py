import pandas as pd
import numpy as np 
import plotly.express as px
# import matplotlib.pyplot as plt
import pingouin as pg
from functions.data_assist import apply_mapping, rename_columns

#%% reset working directory
import os
print(os.getcwd())
wd = '/Users/kristiinajoon/Documents/Projects/Qualtrics'
os.chdir(wd)



#%% ############################# read data ##################################

df = pd.read_csv('data/raw_conjoint.csv')

# check data 
pd.set_option('display.max_columns', None) # displays all columns when printing parts of the df
columns = df.columns.tolist()
df = df.drop([0, 1]) # remove first two rows



# %% ############################# clean data ################################

# fix typos
df.rename(columns={'languge': 'language'}, inplace=True)

# fix data types
df['Finished'] = df['Finished'].replace(
    {'true': True, 'True': True, 'false': False, 'False': False}
).astype(bool)

columns_to_num = ['Duration (in seconds)', 
                  'household-size', 
                  'trust_1', 
                  'trust_2',
                  'trust_3', 
                  'satisfaction_1', 
                  'literacy6_5']
for col in columns_to_num:
    df[col] = pd.to_numeric(df[col], errors='coerce')

# add column for IDs and duration in min
df['ID'] = range(1, len(df) + 1)
df['duration_min'] = df['Duration (in seconds)'] / 60
df['duration_min'].round(3) # do I need to store it in df['dur...'] as well?

# filter out incompletes
df = df[df['DistributionChannel'] != 'preview'] # filter out previews
df = df[df['Finished'] == True] # filter out recorded incompletes
df = df.dropna(subset=['canton']) # filter out quota fulls

# speeders and laggards
# calculate the 5% and 99% quantiles
lower_threshold = df['duration_min'].quantile(0.05)
upper_threshold = df['duration_min'].quantile(0.99)
print(f"Lower threshold (lowest 5% quartile): {lower_threshold} minutes")
print(f"Upper threshold (highest 1% quartile): {upper_threshold} minutes")
df['speeder'] = df['duration_min'] < lower_threshold
df['laggard'] = df['duration_min'] > upper_threshold 

# inattentives based on justice section (exact same answer for all questions)
just_columns = ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1', 
           'justice-general_2', 'justice-tax_2', 'justice-subsidy_2', 
           'justice-general_3', 'justice-tax_3', 'justice-subsidy_3', 
           'justice-general_4', 'justice-tax_4', 'justice-subsidy_4'
]
attention_mask = (df[just_columns].nunique(axis=1) == 1)
df['inattentive'] = attention_mask

# count the number of rows where the attention filters are True
print(f"Number of speeders (5% fastest): {df['speeder'].sum()}")
print(f"Number of laggards (1% slowest): {df['laggard'].sum()}")
print(f"Number of inattentive respondents: {df['inattentive'].sum()}")

# filter out rows of speeders, laggards, or inattentives
df_filtered = df[~((df['speeder'] == True) | 
                   (df['laggard'] == True) |
                   (df['inattentive'] == True)
                  )]

# remove non-functional empty columns 
empty_columns = [col for col in df.columns if col.endswith('_Table')]
df = df.drop(columns=empty_columns)

# rename columns for pv experiment
df = rename_columns(df, 'TargetMix', 'mix')
df = rename_columns(df, 'Imports', 'imports')
df = rename_columns(df, 'RooftopSolarPV', 'pv')
df = rename_columns(df, 'Infrastructure', 'tradeoffs')
df = rename_columns(df, 'Distribution', 'distribution')



# %% ########################## recode demographics ##############################

# recode likert scales in conjoints
numerical_values = [0, 1, 2, 3, 4, 5]
binary_values = [0, 0, 0, 1, 1, 1]
numerical_values_neg = [-3, -2, -1, 1, 2, 3]
rating_values = ['Stark dagegen',
                 'Dagegen',
                 'Eher dagegen',
                 'Eher dafür',
                 'Dafür',
                 'Stark dafür']
rating_scale = np.array(list(zip(rating_values, binary_values)))
likert_dict = {**dict(rating_scale)}
df = apply_mapping(df, likert_dict, column_pattern=['justice', 'rating'])

# recode demographic values

demographics_dict = {
    # gender
    "Weiblich": "female",
    "Männlich": "male", 
    "Nicht-binär": "non-binary",

    # age
    "18-39 Jahre": "18-39", 
    "40-64 Jahre": "40-64", 
    "65-79 Jahre": "65-79", 
    "80 Jahre oder älter": "80+",

    # language region
    "Deutschsprachige Schweiz": "german", 
    "Französischsprachige Schweiz": "french", 
    "Italienischsprachige Schweiz": "italian",
    "Rätoromanische Schweiz": "romansh",

    # survey language
    "Deutsch": "german",
    "Französisch": "french",
    "Italienisch": "italian",

    # income
    "Unter CHF 70,000": "low", # lower
    "CHF 70,000 – CHF 100,000": "mid", # lower
    "CHF 100,001 – CHF 150,000": "mid", # higher
    "CHF 150,001 – CHF 250,000": "high", # higher
    "Über 250,000": "high", # higher
    "Möchte ich nicht sagen": np.nan, 

    # education 
    "Keine Matura": "no secondary",
    "Matura oder Berufsausbildung": "secondary",
    "Abschluss einer Fachhochschule oder Universität": "university",

    # citizen 
    "Ja": True, 
    "Nein": False, 

    # tenant
    "Mieter:in": True, 
    "Besitzer:in": False,

    # urbanness
    "Stadt": "city",
    "Agglomeration": "suburb",
    "Land": "rural",

    # political orientation
    "Grüne Partei der Schweiz (GPS)": "left", 
    "Sozialdemokratische Partei der Schweiz (SP)": "left", 
    "Grünliberale Partei (GLP)": "liberal", 
    "Die Mitte (ehemals CVP/BDP)": "liberal", 
    "Die Liberalen (FDP)": "conservative", 
    "Schweizerische Volkspartei (SVP)": "conservative", 
    "Andere": np.nan, 
    "Keine": np.nan, 
    "Möchte ich nicht sagen": np.nan

    #TODO energy literacy
}

# apply mapping to columns whose names contain 'table'
df = apply_mapping(df, demographics_dict)

# household size ?
# satisfaction?

# create categorical political trust and governmental satisfaction
df = df.copy() # reduce fragmentation
df['trust_mean'] = pd.concat([df['trust_1'], df['trust_2'], df['trust_3']], axis=1).mean(axis=1).round(3)
df['trust'] = pd.cut(df['trust_mean'], 
                              bins=[-float('inf'), 
                                    df['trust_mean'].quantile(0.33), 
                                    df['trust_mean'].quantile(0.65), # ensures ~33% in each bin
                                    float('inf')], 
                              labels=['low', 'mid', 'high'], 
                              include_lowest=True)

df['satisfaction'] = pd.cut(df['satisfaction_1'], 
                              bins=[-float('inf'), 
                                    df['satisfaction_1'].quantile(0.27), 
                                    df['satisfaction_1'].quantile(0.63), # ensures ~33% in each bin
                                    float('inf')], 
                              labels=['low', 'mid', 'high'], 
                              include_lowest=True)

# %% ################################# recode justice ##############################

# recode justice likert scales
likert_values = ['Stimme überhaupt nicht zu', 
                 'Stimme nicht zu', 
                 'Stimme eher nicht zu', 
                 'Stimme eher zu', 
                 'Stimme zu', 
                 'Stimme voll und ganz zu']
likert_scale = np.array(list(zip(likert_values, numerical_values_neg)))
justice_dict = {**dict(likert_scale)}
df_justice = apply_mapping(df, justice_dict, column_pattern=['justice', 'rating'])

# justice columns dictionary
justice_columns = {
    'utilitarian': ['justice-general_1', 'justice-tax_1', 'justice-subsidy_1'], 
    'egalitarian': ['justice-general_2', 'justice-tax_2', 'justice-subsidy_2'], 
    'sufficientarian': ['justice-general_3', 'justice-tax_3', 'justice-subsidy_3'], 
    'limitarian': ['justice-general_4', 'justice-tax_4', 'justice-subsidy_4']
}

# convert columns to numeric types
for just_columns in justice_columns.values():
    for col in just_columns:
        df_justice[col] = pd.to_numeric(df[col], errors='coerce')

# get mean for each key and append to dataframe
for key, just_columns in justice_columns.items():
    df[key] = df[just_columns].sum(axis=1).round(3)

# # for binary scale values, append sum to df
# for key, cjust_olumns in justice_columns.items():
#     df_justice[key] = df_justice[just_columns].sum(axis=1)

#TODO check if the principles were already added to df before
lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian', 
                       'speeder', 
                       'laggard', 
                       'inattentive']]
lpa_data.to_csv('data/lpa_input.csv', index=False)

lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian',
                       'speeder', 
                       'laggard', 
                       'inattentive']]
lpa_data.to_csv('data/lpa_input.csv', index=False)

# now run lpa analysis

# %% ################### check ICC for justice ##########################

lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian', 
                       'justice-general_1', 
                       'justice-tax_1', 
                       'justice-subsidy_1',
                       'justice-general_2', 
                       'justice-tax_2',
                       'justice-subsidy_2',
                       'justice-general_3',
                       'justice-tax_3',
                       'justice-subsidy_3',
                       'justice-general_4',
                       'justice-tax_4',
                       'justice-subsidy_4',
                       'speeder', 
                       'laggard', 
                       'inattentive']]


lpa_data = lpa_data[(df_justice['speeder'] == False) & 
              (df_justice['laggard'] == False) & 
              (df_justice['inattentive'] == False)]

df_long = lpa_data.melt(id_vars=['ID'], 
                        value_vars=['justice-general_1', 'justice-tax_1', 'justice-subsidy_1',
                                    'justice-general_2', 'justice-tax_2', 'justice-subsidy_2',
                                    'justice-general_3', 'justice-tax_3', 'justice-subsidy_3',
                                    'justice-general_4', 'justice-tax_4', 'justice-subsidy_4'],
                        var_name='variable', value_name='score')

# Extract set number from variable name (1, 2, 3, or 4)
df_long['set'] = df_long['variable'].str.extract(r'(\d+)')

icc_results = pg.intraclass_corr(data=df_long, 
                                 targets='ID', 
                                 raters='set', 
                                 ratings='score')

print(icc_results)

# %% ######################################### check sample #################################################

# basics
print(df.info())
print(df.describe())

# gender counts
fig = px.histogram(df, x="gender", histnorm='percent')
fig.show()

# age counts
fig = px.histogram(df, x="age", histnorm='percent')
fig.show()

# language region
fig = px.histogram(df, x="region", histnorm='percent')
fig.show()

#TODO make list of descriptions
