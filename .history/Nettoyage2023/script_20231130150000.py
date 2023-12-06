import pandas as pd

# Remplacez par le chemin de votre fichier
data_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

# print("Affichage des 5 premières lignes", data_2023.head())

print("Affichage de nombre de lignes et de colonnes", data_2023.shape)

print("Affiche les types de données de chaque colonne", data_2023.dtypes)

print("un résumé, y compris le nombre de non-null pour chaque colonne", data_2023.info())
