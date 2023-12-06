import pandas as pd

# Remplacez par le chemin de votre fichier
df = pd.read_csv('./Nettoyage2023/script.csv')

print("Affichage des 5 premières lignes", df.head())

print("",df.shape)  # Affiche le nombre de lignes et de colonnes

print(df.dtypes)  # Affiche les types de données de chaque colonne

# Fournit un résumé, y compris le nombre de non-null pour chaque colonne
print(df.info())
