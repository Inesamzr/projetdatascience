import pandas as pd

# Charger le jeu de données
df = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

print("Affichage de nombre de lignes et de colonnes", data_drop_2023.shape)


# Sélectionner les colonnes spécifiques
colonnes_a_garder = ['Date', 'Genre',
                     'identifiant', 'Année obtention du diplôme', 'Formation']
data_2023_axe1 = df[colonnes_a_garder]

# Continuer avec d'autres opérations ETL...
