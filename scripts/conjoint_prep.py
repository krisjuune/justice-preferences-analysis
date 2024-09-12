import pandas as pd
from functions.conjoint_assist import prep_conjoint
from functions.data_assist import apply_mapping

# run this after LPA 

# %% ################################## translate conjoints #######################################

translation_dict_heat = {
    # ban
    "Kein Verbot": "No ban",
    "Pas d'interdiction": "No ban",
    "Nessun divieto": "No ban",

    "Verbot von Neuinstallationen": "Ban on new installations",
    "Interdiction de nouvelles installations uniquement": "Ban on new installations",
    "Divieto di installare nuovi boiler": "Ban on new installations",

    "Verbot von Neuinstallationen und obligatorischer Austausch bestehender fossilen Heizungen": "Ban and fossil heating replacement",
    "Interdiction de nouvelles installations et remplacement obligatoire des chauffages à combustibles fossiles existants": "Ban and fossil heating replacement",
    "Divieto di installare nuovi boiler e sostituzione obbligatoria dei boiler esistenti": "Ban and fossil heating replacement",

    # heat pump
    "Wärmepumpe mit Subventionen kaufen": "Subsidy", 
    "Achat d’une pompe à chaleur avec des subventions": "Subsidy",
    "Acquisto di una pompa di calore con sovvenzioni": "Subsidy",

    "Wärmepumpe von der Regierung leasen": "Governmental lease",
    "Achat d’une pompe à chaleur en leasing auprès du gouvernement": "Governmental lease",
    "Leasing di una pompa di calore di proprietà del governo": "Governmental lease",

    "Wärmepumpen-Abo": "Subscription",
    "Abonnement à une pompe à chaleur": "Subscription",
    "Abbonamento ad una pompa di calore": "Subscription",

    # building codes
    "Neue Gebäude müssen energieeffizient sein": "New buildings must be energy efficient", 
    "Les nouveaux bâtiments doivent être énergétiquement efficaces": "New buildings must be energy efficient",
    "Nuovi edifici devono rispettare standard di alta efficienza energetica": "New buildings must be energy efficient",

    "Neue Gebäude müssen energieeffizient sein und vor Ort erneuerbaren Strom erzeugen": "New buildings must be energy efficient and produce renewable electricity on-site",
    "Les nouveaux bâtiments doivent être énergétiquement efficaces et produire de l'électricité renouvelable sur place": "New buildings must be energy efficient and produce renewable electricity on-site",
    "Nuovi edifici devono rispettare standard di alta efficienza energetica e produrre elettricità rinnovabile in modo autonomo": "New buildings must be energy efficient and produce renewable electricity on-site",

    "Alle Gebäude müssen energieeffizient sein": "All buildings need to be energy efficient",
    "Tous les bâtiments doivent être énergétiquement efficaces": "All buildings need to be energy efficient",
    "Tutti gli edifici devono rispettare standard di alta efficienza energetica": "All buildings need to be energy efficient",

    "Alle Gebäude müssen energieeffizient sein und vor Ort erneuerbaren Strom erzeugen": "All buildings need to be energy efficient and produce renewable electricity on-site",
    "Tous les bâtiments doivent être énergétiquement efficaces et produire de l'électricité renouvelable sur place": "All buildings need to be energy efficient and produce renewable electricity on-site",
    "Tutti gli edifici devono rispettare standard di alta efficienza energetica e produrre elettricità rinnovabile in modo autonomo": "All buildings need to be energy efficient and produce renewable electricity on-site",
    
    # exemptions -- there's an error here somewhere
    "Keine Ausnahmen": "No exemptions", 
    "Pas d'exemption": "No exemptions",
    "Nessuna eccezione": "No exemptions",

    "Geringverdienende Haushalte sind ausgenommen": "Low-income households are exempted",
    "Les ménages à revenus faibles sont exclus": "Low-income households are exempted",
    "Sono esentate le famiglie e utenze a basso reddito": "Low-income households are exempted",

    "Gering- und mittelverdienende Haushalte sind ausgenommen": "Low and middle-income households are exempted",
    "Les ménages à revenus faibles et moyens sont exclus": "Low and middle-income households are exempted",
    "Sono esentate le famiglie e utenze a basso e medio reddito": "Low and middle-income households are exempted"
}

translate_dict_pv = {
    # target mix
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Xuqo08nWGvzTaSr': 'More hydro',
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_lwjCDBh17ODzYQM': 'More hydro', 
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_FvSefnnxSgWbb8J': 'More hydro', 

    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_PnFZWmknO1NZLvB': 'More solar', 
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_vCbbVKg7jmWJgva': 'More solar', 
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_WFCdHR97e3KUwQG': 'More solar', 

    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9LCSI0Qu1yQuHNY': 'More wind',
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9dSwpo1C4dEgjHD': 'More wind', 
    'https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_G9HNH3uNGMuVtEb': 'More wind', 

    # rooftop pv requirements
    'Keine Verpflichtungen': 'No obligation', 
    'Nessun obbligo': 'No obligation', 
    'Aucune obligation': 'No obligation', 

    'Neuen öffentlichen und gewerblichen Gebäuden': 'New public and commercial buildings', 
    'Les nouveaux bâtiments publics et commerciaux': 'New public and commercial buildings', 
    'Nuovi edifici pubblici e commerciali': 'New public and commercial buildings',

    'Neuen und existierenden öffentlichen und gewerblichen Gebäuden': 'New and existing public and commercial buildings', 
    'Les bâtiments publics et commerciaux à la fois nouveaux et existants': 'New and existing public and commercial buildings', 
    'Edifici pubblici e commerciali sia nuovi che esistenti': 'New and existing public and commercial buildings', 

    'Allen neuen Gebäuden': 'All new buildings', 
    'Tous les nouveaux bâtiments': 'All new buildings', 
    'Tutti i nuovi edifici': 'All new buildings', 

    'Allen neuen und existierenden Gebäuden': 'All new and existing buildings', 
    'Tous les bâtiments neufs et existants': 'All new and existing buildings', 
    'Tutti gli edifici nuovi ed esistenti': 'All new and existing buildings', 

    # biodiversity tradeoffs
    'Keine Ausnahmefälle': 'No trade-offs',
    'Pas de cas exceptionnels': 'No trade-offs', 
    'In nessun caso eccezionale': 'No trade-offs', 

    'Alpenregionen': 'Alpine regions',
    'Les régions alpines': 'Alpine regions', 
    'Regioni alpine': 'Alpine regions', 

    'Landwirtschaflichen Flächen': 'Agricultural areas',
    'Les terres agricoles': 'Agricultural areas', 
    'Superfici agricole': 'Agricultural areas',

    'Wäldern': 'Forests',
    'Les forêts': 'Forests', 
    'Foreste': 'Forests', 

    'Flüssen': 'Rivers',
    'Les rivières': 'Rivers', 
    'Fiumi': 'Rivers',

    'Seen': 'Lakes', 
    'Les lacs': 'Lakes', 
    'Laghi': 'Lakes',

    # cantonal distribution
    'Keine Vorgabe': 'No agreed distribution', 
    'Pas d\'objectif': 'No agreed distribution', 
    'Nessun obiettivo': 'No agreed distribution', 

    'Basierend auf dem Erzeugungspotenzial': 'Potential-based', 
    'Basée sur la production maximale potentielle d’un canton': 'Potential-based', 
    'In base al potenziale di un cantone': 'Potential-based', 

    'Basierend auf der Bevölkerungszahl': 'Equal per person', 
    'Basée sur le nombre de personnes vivant dans chaque canton': 'Equal per person',
    'In base al numero di abitanti di ogni cantone': 'Equal per person', 

    'Mindestensvorgabe pro Kanton': 'Minimum limit', 
    'Un minimum de production par canton est établi': 'Minimum limit', 
    'In base al livello di produzione minimo cantonale concordato': 'Minimum limit', 

    'Deckelung pro Kanton': 'Maximum limit',
    'Un maximum de production par canton est établi': 'Maximum limit',
    'Nessun cantone produce più di un tetto massimo concordato': 'Maximum limit'
}

# apply mapping to columns whose names contain 'table'
conjoint_dict = translation_dict_heat | translate_dict_pv
df = apply_mapping(df, conjoint_dict, column_pattern='table')

#TODO why are there tables saved for everyone for pv experiment even for those who did not participate in that one



# %% ############################# add lpa data #######################################

lpa_cont = pd.read_csv('data/lpa_data.csv')
# lpa_bin = pd.read_csv('data/lpa_data_bi.csv')

just_str = ['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian', 'class']

# for original_str in just_str:
#     cont_str = f'{original_str}_cont' 
#     bin_str = f'{original_str}_bin' 
#     lpa_cont = rename_columns(lpa_cont, original_str, cont_str)
    # lpa_bin = rename_columns(lpa_bin, original_str, bin_str)

# df = df.merge(
#     lpa_bin[['ID', 
#              'utilitarian_bin', 
#              'egalitarian_bin', 
#              'sufficientarian_bin', 
#              'limitarian_bin', 
#              'justice_class_bin']],
#     on='ID',
#     how='left'
# )

# add _cont below to columns names if uncommenting the code above for bin values too
df = df.merge(
    lpa_cont[['ID', 
              'utilitarian', 
              'egalitarian', 
              'sufficientarian', 
              'limitarian', 
              'justice_class']],
    on='ID',
    how='left'
)

# %% ############################ conjoint data #######################################

# select respondent data
respondents = df[[
        "ID", "duration_min", "gender", "age", "region", "canton", "citizen", 
        "education", "urbanness", "renting", "income", "household-size", "party", 
        "satisfaction_1", "justice_class", "speeder", "laggard", "inattentive", 
        "trust"]] # missing literacy

heat_regex = 'pv|mix|imports|tradeoffs|distribution'
heat_filemarker = 'heat'
pv_regex = 'heat|year|tax|ban|energyclass|exemption'
pv_filemarker = 'pv'

df_heat = prep_conjoint(df, respondent_columns=respondents, regex_list=heat_regex, filemarker=heat_filemarker) #1068 participants in heat
df_pv = prep_conjoint(df, respondent_columns=respondents, regex_list=pv_regex, filemarker=pv_filemarker) #1062 participants in pv


# %%
