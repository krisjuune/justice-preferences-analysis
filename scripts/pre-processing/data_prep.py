import pandas as pd
import numpy as np
from functions.data_assist import apply_mapping, rename_columns


#%% ############################# read data ##################################

df = pd.read_csv('raw_data/raw_conjoint_120624.csv')
pd.set_option('display.max_columns', None) # displays all columns when printing parts of the df
columns = df.columns.tolist()
df = df.drop([0, 1])


# %% ############################# clean data ################################

# fix typos and replace dashes with underscores
df.rename(columns={'languge': 'language'}, inplace=True)
df = rename_columns(df, 'justice-', 'justice_')

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
df['duration_min'] = (df['Duration (in seconds)'] / 60).round(3)

# filter out previews
df = df[df['DistributionChannel'] != 'preview'] 
# filter out recorded incompletes
df = df[df['Finished'] == True] 
# filter out quota fulls
df = df.dropna(subset=['canton']) 

# speeders and laggards
# calculate the 5% and 99% quantiles
lower_threshold = df['duration_min'].quantile(0.05)
upper_threshold = df['duration_min'].quantile(0.95)
print(f"Lower threshold (lowest 5% quartile): {lower_threshold} minutes")
print(f"Upper threshold (highest 5% quartile): {upper_threshold} minutes")
df['speeder'] = df['duration_min'] < lower_threshold
df['laggard'] = df['duration_min'] > upper_threshold 

# inattentives based on justice section (exact same answer for all questions)
just_columns = ['justice_general_1', 'justice_tax_1', 'justice_subsidy_1', 
           'justice_general_2', 'justice_tax_2', 'justice_subsidy_2', 
           'justice_general_3', 'justice_tax_3', 'justice_subsidy_3', 
           'justice_general_4', 'justice_tax_4', 'justice_subsidy_4'
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

# add column for which experiment
df['experiment'] = np.where(df['7_heat-choice'].notna(), 'heat',
                   np.where(df['7_pv-choice'].notna(), 'pv', np.nan))


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
rating_scale = np.array(list(zip(rating_values, numerical_values)))
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

#TODO household size ?

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
likert_scale = np.array(list(zip(likert_values, numerical_values)))
justice_dict = {**dict(likert_scale)}
df_justice = apply_mapping(df, justice_dict, column_pattern=['justice', 'rating'])

# justice columns dictionary
justice_columns = {
    'utilitarian': ['justice_general_1', 'justice_tax_1', 'justice_subsidy_1'], 
    'egalitarian': ['justice_general_2', 'justice_tax_2', 'justice_subsidy_2'], 
    'sufficientarian': ['justice_general_3', 'justice_tax_3', 'justice_subsidy_3'], 
    'limitarian': ['justice_general_4', 'justice_tax_4', 'justice_subsidy_4']
}

# convert columns to numeric types
for just_columns in justice_columns.values():
    for col in just_columns:
        df_justice[col] = pd.to_numeric(df[col], errors='coerce')

# get mean for each key and append to dataframe
for key, just_columns in justice_columns.items():
    df[key] = df[just_columns].sum(axis=1).round(3)

lpa_data = df_justice[['ID', 
                       'utilitarian', 
                       'egalitarian', 
                       'sufficientarian', 
                       'limitarian', 
                       'speeder', 
                       'laggard', 
                       'inattentive', 
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
                       'justice_subsidy_4']]

lpa_data.to_csv('data/lpa_input.csv', index=False)

# now run lpa analysis

# %% ######################################### check sample #################################################

# filter out speeders, laggards, inattentives
df_filtered = df[~df[['speeder', 'laggard', 'inattentive']].any(axis=1)]

# get sample descriptions
columns_to_summarize = ['experiment', 'age', 'gender', 'region']

summary_table = {}
for col in columns_to_summarize:
    counts = df[col].value_counts(dropna=True)  # Count non-NaN values
    percentages = df[col].value_counts(normalize=True, dropna=True) * 100  # Get percentages

    summary_table[col] = pd.DataFrame({'Count': counts, 'Percentage': percentages})

# Display each summary table
for col, table in summary_table.items():
    print(f"Summary for {col}:\n")
    print(table, "\n")


# %%
