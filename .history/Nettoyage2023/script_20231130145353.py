import pandas as pd

# Remplacez par le chemin de votre fichier
df = pd.read_csv('./Nettoyage2023/script.csv')

print("Affichage des 5 premières lignes", df.head())

print("Affichage de nombre de lignes et de colonnes",df.shape) 

print("df.dtypes)

# Fournit un résumé, y compris le nombre de non-null pour chaque colonne
print(df.info())
