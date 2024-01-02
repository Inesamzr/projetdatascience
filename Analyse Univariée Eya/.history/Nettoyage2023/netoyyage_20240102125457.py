import pandas as pd

# Charger le jeu de données
df = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

print("Affichage de nombre de lignes et de colonnes", data_drop_2023.shape)


# Sélectionner les colonnes spécifiques
colonnes_a_garder = ['Date', 'Genre',
                     'identifiant', 'Année obtention du diplôme', 'Formation', 'Votre rémunération brute annuelle (hors primes )?',
                     'Vous percevez des primes ?', 'Votre rémunération brute annuelle AVEC primes',
                     'Quel est votre salaire BRUT ANNUEL (HORS PRIMES)?', 
                     'Percevez vous des primes et/ou un 13ème mois ?',
                     'Quel est votre salaire brut ANNUEL AVEC PRIMES ?',
                     'Quel était votre rémunération brute annuelle (HORS PRIMES) ?', #premier emploi
                     'Perceviez-vous des primes ?',#premier emploi
                     'Quel était votre rémunération brut AVEC PRIMES ?'
                     
                     ]
data_2023_axe1 = df[colonnes_a_garder]

# Continuer avec d'autres opérations ETL...
