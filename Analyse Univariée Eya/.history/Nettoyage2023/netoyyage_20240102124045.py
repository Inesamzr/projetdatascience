import pandas as pd

# Charger le jeu de données
data_2023_axe1 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

print("Affichage de nombre de lignes et de colonnes", data_drop_2023.shape)


# Sélectionner les colonnes spécifiques
colonnes_a_garder = ['Colonne1', 'Colonne2', 'Colonne3']
df = data[colonnes_a_garder]

# Continuer avec d'autres opérations ETL...
