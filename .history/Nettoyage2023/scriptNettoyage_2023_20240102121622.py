import pandas as pd

# Remplacez par le chemin de votre fichier
data_drop_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

print("Affichage de nombre de lignes et de colonnes", data_drop_2023.shape)
#colonnes = data_drop_2023.columns
#print(colonnes)

# lister les colonnes à supprimer :
colonnes_a_supprimer = ['Quel type d\'études poursuivez-vous ?',
                        'Concours préparé(s)', 'Dans quel établissement êtes-vous inscrit·e (nom et ville éventuellement) ?',
                        'Pour quelle raison avez-vous principalement choisi de poursuivre des études ?',
                        'Quel type de thèse préparez-vous ?',
                        'Nom et coordonnées de l\'entreprise partenaire', 'Sujet de votre Thèse / PhD',
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
                        'Lequel ou lesquels ?',
                        'Le site web s\'il existe ?',
                        'Disposez-vous d\'une RQTH (Reconnaissance de la qualité de Travailleur Handicapé)?',
                        'Adresse ( numero, rue, avenue..) de l\'entreprise',
                        'Code postal de l\'entreprise',
                        ' Son site internet ?',
                        'Pourquoi considérez vous votre emploi comme directement lié aux energies renouvelables?',
                        'Quels sont les aspects du Developpement Durable/Responsabilité sociétale qui vous concernent dans votre mission?',
                        'Votre diplôme vous a permis d\'acquérir les compétences demandées par votre employeur en matière de transformations environnementales?',
                        'Les enjeux de la transition écologique font partie des préoccupations de votre employeur / votre entreprise ?',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Français',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Anglais',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Allemand',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Espagnol',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Chinois',
                        'Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Autre langue',
                        'Pour quelle(s) raison(s) recherchez vous un autre emploi ? ',
                        'Quel type d\'études poursuiviez-vous ?',
                        'Concours préparé(s)6',
                        'Pour quelle raison aviez-vous principalement choisi de poursuivre des études ?',
                        'De quel type de volontariat s\'agissait-il ?',
                        'Durée du contrat de volontariat (en mois)',
                        'Pays ou territoire du volontariat7',
                        'Quel type de thèse prépariez-vous ?',
                        'Nom et coordonnées de l\'entreprise partenaire8',
                        'Sujet de Thèse',
                        'Le laboratoire est localisé...',
                        'La thèse a-t-elle joué un rôle prépondérant dans votre insertion professionnelle ?',
                        'A quel stade de la création/reprise d\'entreprise  vous trouviez-vous ?',
                        'Etiez-vous accompagné·e par un incubateur et/ou une pépinière d\'entreprises ?',
                        'Lequel ou lesquels ?10',
                        'Une rapide description de votre activité','Quel etait son secteur d\'activité ?',
                        'Le site web s\'il existe ?11',
                        'Dans quelles conditions s\'est déroulé votre départ de votre première entreprise ?',
                        'Adresse ( numero, rue, avenue..)de l\'entreprise',
                        'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La réputation du département',
                        'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La formation',
                        'L\'accompagnement de votre école dans votre projet professionnel vous a-t-il paru...',
                        'Laquelle ?',
                        'Quels sont vos projets d\'évolution de carrière ?',
                        'Accepteriez-vous d\'être sollicité·e par l\'école pour intervenir dans la formation ?',
                        'Sous quelle(s) forme(s), désireriez-vous intervenir ?',
                        'Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d\'apprentissage par l\'école ?',
                        'Depuis que vous avez quitté l\'école, êtes-vous encore en contact avec des enseignants du département ?',

                        ]
data_drop_2023.drop(columns=colonnes_a_supprimer, inplace=True)

#data_drop_2023.to_csv('enquete_2023_colonnes_supprimées.csv', index=False)

data_drop_2023.rename(columns={'Année obtention du diplôme': 'date_diplome',
                             'Formation': 'filiere',
                             'Genre': 'sexe',
                             'Vous avez la nationalité française?': 'nationalite_francaise',
                             'Quelle est votre nationalité?': 'nationalite',
                             "Pour votre dernière année, vous étiez inscrit·e en :": 'type_formation',
                             "Êtes-vous à la recherche d'un emploi depuis la sortie de l'école ? ": 'en_recherche_emploi',
                             "Avez-vous refusé une ou plusieurs proposition(s) d'emploi depuis ?": 'refus_emploi',
                             "Quelle(s) difficulté(s) rencontriez-vous dans votre recherche d'emploi ?": 'difficultes_recherche',
                             "Vous poursuivez des études en alternance?": "alternance",
                             "Avez-vous eu une autre activité (emploi CDD/CDI, poursuite d'étude, thèse, création d'entreprise) entre votre sortie de l'école et votre situation actuelle ?": "autre_activite",
                             "Sur cet emploi, vous êtes :": "statut_emploi",
                             "Quelle est la nature de votre contrat de travail ?": "nature_contrat",
                             'Quelle est la durée de votre CDD ?': 'duree_CDD',
                             'Votre emploi actuel se trouve ?': 'localisation_emploi',
                             'Dans quelle région ?': 'region_emploi',
                             'Dans quel pays ?': 'pays_localisation',
                             #"Travaillez-vous à temps partiel ?": "temps_partiel",
                             "Vous travaillez": "temps_partiel",
                             "Depuis combien de mois occupez-vous cet emploi ?": "mois_emploi_occupe",
                             "Quel est le nom de l'entreprise dans laquelle vous travaillez ?": "nom_entreprise",
                             "Quelle est la taille de l'entreprise (ou du groupe si l'entreprise appartient à un groupe) dans laquelle (ou lequel) vous travaillez ?": "taille_entreprise",
                             "L'entreprise dans laquelle vous travaillez actuellement appartient-elle à un groupe ?": "entreprise_groupe",
                             "Lequel?": "groupe",
                             "Lequel?16": "groupe",
                             "Dans quel service ou département exercez-vous cet emploi (à défaut de service ou de département, indiquez votre fonction principale) ?": "departement_emploi",
                             "Quel est l'intitulé de votre poste ?": "intitule_emploi",
                             "Avez-vous un statut de cadre ou assimilé ?": "cadre",
                             "Quel est le secteur d'activité de votre entreprise (celle qui vous rémunère)?": "secteur_activite_entreprise",
                             #"Quel est le domaine général de votre entreprise ?": "domaine_general_entreprise",
                             #"Quels sont les langages informatiques que vous pratiquez dans le cadre de votre activité :": "langage_programmation",  
                             "Considérez vous que votre emploi est directement lié aux énergies renouvelables ?": "emploi_lie_energie_renouvelable",
                             "Quel est votre salaire BRUT ANNUEL (HORS PRIMES)?": "remuneration_annuelle_brute",
                             "Percevez vous des primes et/ou un 13ème mois ?": "prime",
                             "Quel est votre salaire brut ANNUEL AVEC PRIMES ?": "remuneration_prime",
                             "Responsabilités exercées - Vous avez des personnes sous votre responsabilité hiérarchique ?": "responsabilite_hierarchie",
                             "Responsabilités exercées - Vous avez la responsabilité d'un budget ?": "responsabilite_budget",
                             "Responsabilités exercées - Vous avez la responsabilité fonctionnelle  d'une équipe ?": "responsabilite_equipe",
                             "Responsabilités exercées -  Vous avez la responsabilité d'un projet ?": "responsabilite ",
                             "Votre fonction est-elle liée à l'international ? ": "international",
                             "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Français]": "langue_français",
                             "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Anglais]": "langue_anglais",
                             "Combien de temps avez-vous mis pour trouver votre emploi actuel ?": "temps_trouve_emploi",
                             "Comment avez-vous trouvé votre emploi actuel ?": "moyen_trouver_emploi",
                             "Vos principaux critères de choix ?": "critere_choix_emploi",
                             "Estimez-vous que votre emploi correspond... - A votre niveau de qualification ?": "emploi_niveau_qualification",
                             "Estimez-vous que votre emploi correspond... - Au secteur disciplinaire de votre formation ?": "emploi_formation",
                             "Globalement, êtes-vous satisfait·e de votre emploi actuel ?": "satisfaction_emploi",
                             "Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos conditions de travail": "condition_travail",
                             "Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos relations avec vos collègues": "relation_collegues",
                             "Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau de rémunération": "niveau_remuneration",
                             "Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau d'autonomie et de responsabilité": "niveau_autonomie",
                             "Comment jugeriez-vous les aspects suivants de votre emploi ? - La localisation géographique de votre emploi": "jugement_localisation",
                             "Vous recherchez un autre emploi ?": "recherche_nouvel_emploi",
                             "Pour quelle(s) raison(s) recherchez vous un autre emploi ? ": "raisons_recherche_emploi",
                             "Quelle était votre situation pour cette 1ère activité qui a fait suite à la sortie de Polytech ?": "situation_sortie_polytech",
                             "Combien de mois avez-vous mis pour trouver votre premier emploi depuis l'obtention de votre diplôme ?": "temps_trouve_premier_emploi",
                             "Sur ce 1er emploi, vous étiez :": "post_premier_emploi", 
                             "Quelle etait la nature de votre contrat de travail ?": "nature_contrat",
                             #"Dans quelles conditions s'est déroulé votre départ de votre première entreprise ?": "condition_fin_premier_contrat",
                             "Quel était l'intitulé du poste occupé ?": "intitule_premier_emploi",
                             #"Votre 1er emploi actuel se trouvait-il ?": "location_premier_emploi",
                             "Dans quelle région ?12": "region_premier_emploi",
                             "Quel est le nom de l'entreprise dans laquelle vous travailliez ?": "nom_entreprise_premier_emploi", 
                             "Quelle est la taille de l'entreprise (ou du groupe si l'entreprise appartient à un groupe) dans laquelle (ou lequel) vous travailliez ?": "taille_entreprise_premier_emploi",
                             "Quel est le secteur d'activité de cette entreprise ?": "secteur_activite_premier_emploi",
                             "L'entreprise dans laquelle vous travailliez appartient-elle à un groupe ?": "premiere_entreprise_groupe",
                             "Lequel?.1": "premier_groupe",
                             "Quel est le secteur d'activité de cette entreprise ?": "secteur_premiere_entreprise", 
                             "Combien de temps avez vous travaillé dans cette entreprise ? (en mois)": "mois_premiere_entreprise",
                             'Aviez-vous un statut de cadre dans ce premier emploi ?': 'status_cadre_premier_emploi',
                             'Travailliez-vous à temps partiel ?': 'temps_partiel_premier_emploi',
                             'Comment avez-vous trouvé ce premier emploi ?': 'moyen_trouver_premier_emploi',
                             'Quel fut votre principal critère de choix sur ce 1er emploi ?': 'critere_choix_premier_emploi',
                             'Quel était votre rémunération brute annuelle (HORS PRIMES) ?': 'remuneration_annuelle_brute_hors_prime_premier_emploi',
                             'Perceviez-vous des primes et/ou un 13ème mois ?': 'prime_premier_emploi',
                             "Perceviez-vous des primes ?": "prime_premier_emploi",
                             'Quel était votre rémunération brut AVEC PRIMES ?': 'remuneration_annuelle_brute_avec_prime_premier_emploi',
                               "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - La formation,Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - La réputation de l'école/réseau Polytech": "reputation_ecole_pour_premier_emploi",
                             "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Réseau des anciens (Polytech connect)": "reseau_ancien_pour_premier_emploi",
                             "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Stage de 4ème année": "stage_4eme_pour_premier_emploi",
                             "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Stage de 5ème année/Contrat de professionnalisation": "stage_5eme_pour_premier_emploi",
                             "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Vos compétences linguistiques": "competences_linguistiques_pour_premier_emploi",
                             "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Vos experiences à l'étranger " : "experience_etranger_pour_premier_emploi",
                             'Globalement, vous êtes satisfait(e) de votre premier emploi?': 'satisfaction_premier_emploi',
                             'Globalement sur une échelle allant de 1 à 5, êtes-vous satisfait·e de votre formation à Polytech Montpellier ? ': 'satisfaction_formation',
                             'Votre double compétence en Informatique & Gestion vous semble-t-elle  constituer un plus pour votre insertion professionnelle ? ': 'ig_avantage_double_competence',
                             "Etes-vous membre de réseaux ou associations  professionnelles liées à l informatique  ? ": 'ig_membre_reseau',
                             'Quels sont vos projets d Èvolution de carriËre ?': 'projet_evolution_carriere',
                             'Recommanderiez-vous Polytech Montpellier à un·e ami·e qui souhaite poursuivre un cursus dans l enseignement supérieur ?': 'recommandation_polytech',
                             "Votre rémunération brute annuelle (hors primes )?": "remuneration_annuelle_brute_hors_prime",
                             }, inplace=True)

data_drop_2023.to_csv('enquete_2023DS-2_NR2.csv', index=False)

