import pandas as pd

# Remplacez par le chemin de votre fichier
data_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

# print("Affichage des 5 premières lignes", data_2023.head())

print("Affichage de nombre de lignes et de colonnes", data_2023.shape)
colonnes = data_2023.columns
print(colonnes)

# Si on veut lister les colonnes à supprimer :
# colonnes_a_supprimer = ['colonne1', 'colonne2', ...]
# data.drop(columns=colonnes_a_supprimer, inplace=True)
