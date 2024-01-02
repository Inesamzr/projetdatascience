import pandas as pd

# Charger le jeu de données
data_2023_axe1 = pd.read_csv('chemin_du_fichier.csv')

# Sélectionner les colonnes spécifiques
colonnes_a_garder = ['Colonne1', 'Colonne2', 'Colonne3']
df = df[colonnes_a_garder]

# Continuer avec d'autres opérations ETL...
