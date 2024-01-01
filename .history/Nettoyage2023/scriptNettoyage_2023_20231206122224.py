import pandas as pd

# Remplacez par le chemin de votre fichier
data_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

# print("Affichage des 5 premières lignes", data_2023.head())

print("Affichage de nombre de lignes et de colonnes", data_2023.shape)
colonnes = data_2023.columns
print(colonnes)

# lister les colonnes à supprimer :
colonnes_a_supprimer = ['Quel type d\'études poursuivez-vous ?',
                        'Concours préparé(s)', 'Dans quel établissement êtes-vous inscrit·e (nom et ville éventuellement) ?',
                        'Pour quelle raison avez-vous principalement choisi de poursuivre des études ?',
                        'Quel type de thèse préparez-vous ?',
                        'Nom et coordonnées de l\'entreprise partenaire;Sujet de votre Thèse / PhD',
                        'Nom et coordonnées du laboratoire qui vous accueille',
                        'Le laboratoire est-il localisé?',
                        'Région de localisation du laboratoire',
                        'Pays de localisation du laboratoire?',
                        'Vous bénéficiez d\'une source de financement contractuel, d\'une allocation ou d\'un contrat doctoral ?',
                        'Quel est le type de financement contractuel qui permet de financer votre thèse ?',
                        'De quel type de volontariat s\'agit-il ?',
                        'Durée du contrat de volontariat (en mois)',
                        'Pays ou territoire du volontariat',
                        'Quel est le montant brut annuel de l\'allocation que vous percevez ?<br />',
                        'Accompagnement par un incubateur et/ou une pépinière d\'entreprise ?',
                        'Lequel ou lesquels ?'
                        
                        
                        
                        ]
data_2023.drop(columns=colonnes_a_supprimer, inplace=True)
