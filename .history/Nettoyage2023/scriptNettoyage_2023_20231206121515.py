import pandas as pd

# Remplacez par le chemin de votre fichier
data_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

# print("Affichage des 5 premières lignes", data_2023.head())

print("Affichage de nombre de lignes et de colonnes", data_2023.shape)
colonnes = data_2023.columns
print(colonnes)

# lister les colonnes à supprimer :
colonnes_a_supprimer = ['Quel type d\'études poursuivez-vous ?',
                        'Concours préparé(s)', 'Dans quel établissement êtes-vous inscrit·e (nom et ville éventuellement) ?',']
data_2023.drop(columns=colonnes_a_supprimer, inplace=True)
