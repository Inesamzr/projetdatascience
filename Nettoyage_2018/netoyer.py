import pandas as pd

donnees_2018 = pd.read_csv('enquete_2018DS-2.csv', sep=';')

donnees_2018.rename(columns={               'Année obtention du diplôme': 'date_diplome', 
                                            'Formation': 'filiere', 
                                            'Sexe' : 'sexe',
                                            'Vous avez la nationalité française?': 'nationalite_francaise', 
                                            'Quelle est votre nationalité?': 'nationalite', 
                                            'Pour votre dernière année, vous etiez inscrit(e) en :': 'type_formation',
                                            'Quelle est votre situation actuelle ?' : 'situation_actuelle',
                                            "Êtes-vous à la recherche d'un emploi depuis la sortie de l'école ? ": 'en_recherche_emploi',
                                            "Avez-vous refusé une ou plusieurs proposition(s) d'emploi depuis ?" : 'refus_emploi',
                                            "Quelle(s) difficulté(s) rencontrez-vous dans votre recherche d'emploi?" : 'difficultes_recherche',
                                            "vous poursuivez des études en alternance?" : "alternance",
                                            "Avez-vous eu une autre activité (emploi CDD/CDI, poursuite d'étude, thèse, création d'entreprise) entre votre sortie de l'école et votre situation actuelle ?" : "autre_activite",
                                            "Sur cet emploi, vous êtes :" : "statut_emploi",
                                            "Quelle est la nature de votre contrat de travail ?" : "nature_contrat",
                                            'Quelle est la durée de votre CDD?': 'duree_CDD',
                                            'Votre emploi actuel se trouve ?': 'localisation_emploi',
                                            'Dans quelle région?': 'region_emploi',
                                            'Dans quel pays?': 'pays_localisation',
                                            "Travaillez-vous à temps partiel ?" : "temps_partiel",
                                            "Depuis combien de mois occupez-vous cet emploi ?" : "mois_emploi_occupe",
                                            "Quel est le nom de l'entreprise dans laquelle vous travaillez ?" : "nom_entreprise",
                                            "Quelle est la taille de l'entreprise (ou du groupe si l'entreprise appartient à un groupe) dans laquelle (ou lequel) vous travaillez ?" : "taille_entreprise",
                                            "L'entreprise dans laquelle vous travaillez actuellement appartient-elle à un groupe ?" : "entreprise_groupe",
                                            "Lequel?": "groupe",
                                            "Dans quel service ou département exercez-vous cet emploi (à défaut de service ou de département, indiquez votre fonction principale) ?" : "departement_emploi",
                                            "Quel est l'intitulé de votre poste ?" : "intitule_emploi",
                                            "Avez-vous un statut de cadre ou assimilé ?": "cadre",
                                            "Quel est le secteur d'activité de votre entreprise ?" : "secteur_activite_entreprise",
                                            "Quel est le domaine général de votre entreprise ?" : "domaine_general_entreprise",
                                            "Quels sont les langages informatiques que vous pratiquez dans le cadre de votre activité :" : "langage_programmation",
                                            "Considérez vous que votre emploi est directement lié aux énergies renouvelables ?" : "emploi_lie_energie_renouvelable", 
                                            "Quel est votre salaire BRUT ANNUEL HORS PRIMES et HORS 13ème mois ?" : "remuneration_annuelle_brute",
                                            "Percevez vous des primes et/ou un 13ème mois?" : "prime",
                                            "Quel est votre salaire brut ANNUEL AVEC PRIMES et/ou 13ème mois ?" : "remuneration_prime",
                                            "Responsabilités exercées [Vous avez des personnes sous votre responsabilité hiérarchique ?]" : "responsabilite_hierarchie",
                                            "Responsabilités exercées [Vous avez la responsabilité d'un budget ?]": "responsabilite_budget",
                                            "Responsabilités exercées [Vous avez la responsabilité d'une équipe ?]" : "responsabilite_equipe",
                                            "Responsabilités exercées [ Vous avez la responsabilité d'un projet ?]" : "responsabilite ",
                                            "Votre fonction est-elle liée à l'international ? " : "international",
                                            "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Français]" : "langue_français",
                                            "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Anglais]" : "langue_anglais",
                                            "Combien de temps avez-vous mis pour trouver votre emploi actuel?" : "temps_trouve_emploi",
                                            "Comment avez-vous trouvé votre emploi actuel?": "moyen_trouver_emploi",
                                            "Quel a été votre principal critère de choix ?": "critere_choix_emploi",
                                            "Estimez-vous que votre emploi correspond [A votre niveau de qualification ?]" : "emploi_niveau_qualification",
                                            "Estimez-vous que votre emploi correspond [Au secteur disciplinaire de votre formation ?]" : "emploi_formation",
                                            "Globalement, êtes-vous satisfait(e) de votre emploi actuel ?" : "satisfaction_emploi",
                                            "Comment jugeriez-vous les aspects suivants de votre emploi ? [Vos conditions de travail]" : "condition_travail",
                                            "Comment jugeriez-vous les aspects suivants de votre emploi ? [Vos relations avec vos collègues]" : "relation_collegues",
                                            "Comment jugeriez-vous les aspects suivants de votre emploi ? [Votre niveau de rémunération]" : "niveau_remuneration",
                                            "Comment jugeriez-vous les aspects suivants de votre emploi ? [Votre niveau d'autonomie et de responsabilité]" : "niveau_autonomie",
                                            "Comment jugeriez-vous les aspects suivants de votre emploi ? [La localisation géographique de votre emploi]" : "jugement_localisation",
                                            "Vous recherchez un autre emploi?" : "recherche_nouvel_emploi",
                                            "Pour quelle(s) raison(s) recherchez vous un autre emploi? " : "raisons_recherche_emploi",
                                            "Quelle était votre situation pour cette 1ère activité qui a fait suite à la sortie de Polytech ?" : "situation_sortie_polytech",
                                            "Combien de mois avez-vous mis pour trouver votre premier emploi depuis l'obtention de votre diplôme ?" : "temps_trouve_premier_emploi",
                                            "Sur ce 1er emploi, vous étiez :" : "post_premier_emploi",
                                            "Quelle etait la nature de votre contrat de travail ?" : "nature_contrat",
                                            "Dans quelles conditions s'est déroulé votre départ de votre première entreprise ?" : "condition_fin_premier_contrat",
                                            "Quel était l'intitulé du poste occupé ?" : "intitule_premier_emploi",
                                            "Votre 1er emploi actuel se trouvait-il ?" : "location_premier_emploi",
                                            "Dans quelle région?" : "region_premier_emploi",
                                            "Quel est le nom de l'entreprise dans laquelle vous travailliez ?" : "nom_entreprise_premier_emploi",
                                            "Quelle est la taille de l'entreprise (ou du groupe si l'entreprise appartient à un groupe) dans laquelle (ou lequel) vous travailliez ?" : "taille_entreprise_premier_emploi",
                                            "Quel est le secteur d'activité de cette entreprise ?" : "secteur_activite_premier_emploi",
                                            "L'entreprise dans laquelle vous travailliez appartient-elle à un groupe ?" : "premiere_entreprise_groupe",
                                            "Lequel?.1" : "premier_groupe",
                                            "Quel est le secteur d'activité de cette entreprise ?" : "secteur_premiere_entreprise",
                                            "Combien de temps avez vous travaillé dans cette entreprise ?(en mois)" : "mois_premiere_entreprise",
                                            'Aviez-vous un statut de cadre dans ce premier emploi ?' : 'status_cadre_premier_emploi',
                                            'Travailliez-vous à temps partiel ?' : 'temps_partiel_premier_emploi',
                                            'Comment avez-vous trouvé ce premier emploi ?' : 'moyen_trouver_premier_emploi',
                                            'Quel fut votre principal critère de choix sur ce 1er emploi?' : 'critere_choix_premier_emploi',
                                            'Quel était votre rémunération brute annuelle (HORS PRIMES et 13ème mois) ?' : 'remuneration_annuelle_brute_hors_prime_premier_emploi',
                                            'Perceviez vous des primes?' : 'prime_premier_emploi',
                                            'Quel était votre rémunération brut AVEC PRIMES et 13ème mois?' : 'remuneration_annuelle_brute_avec_prime_premier_emploi',
                                            'Globalement, vous êtes satisfait(e) de votre premier emploi?' : 'satisfaction_premier_emploi',
                                            'Globalement,sur une échelle allant de 1 à 5, êtes-vous satisfait(e) de votre formation à Polytech Montpellier ? ' : 'satisfaction_formation',
                                            'Votre double compétence en Informatique & Gestion vous semble-t-elle  constituer un plus pour votre insertion professionnelle ? ' : 'ig_avantage_double_competence',
                                            "Etes-vous membre de réseaux ou associations  professionnelles liées à l'informatique  ? " : 'ig_membre_reseau',
                                            'Quels sont vos projets d Èvolution de carriËre ?' : 'projet_evolution_carriere',
                                            "Recommanderiez-vous Polytech Montpellier à un(e) ami(e) qui souhaite poursuivre un cursus dans l'enseignement supérieur ?" : 'recommandation_polytech',


                            }, inplace=True)

colonnes_a_supprimer = [
    "Depuis combien de mois êtes-vous en recherche d'emploi ? ",
    "Pour quelle(s) raison(s) avez-vous refusé ce ou ces emploi(s) ?",
    "Quel type d'études poursuivez-vous ?",
    "Pour quelle raison avez-vous principalement choisi de poursuivre des études ?",
    "Quel type de thèse préparez-vous ?",
    "Sujet de votre Thèse",
    "Nom et coordonnées du laboratoire qui vous accueille",
    "Vous bénéficiez d'une source de financement contractuel, d'une allocation ou d'un contrat doctoral",
    "Dans quel établissement êtes-vous inscrit(e) (nom et ville éventuellement) ?",
    "Vous bénéficiez d'une source de financement contractuel; d'une allocation ou d'un contrat doctoral",
    "Quel est le type de financement contractuel qui permet de financer votre thèse ?",
    "Votre rémunération brute annuelle (hors primes et 13ème mois)?",
    "Percevez des primes et/ou un 13ème mois?",
    
    'Date',
    'Champ 1',
    'Champ 2',
    'Sans activité volontairement, merci de préciser :',
    "Depuis combien de mois êtes-vous en recherche d emploi ? ",
    "Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage domaine Eau et Génie Civil]",	
    "Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage Mécanique structures industrielles chaudronnées]",
    "Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage Systèmes embarqués]",
    "Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [contrat de professionnalisation]",
    "Depuis combien de temps étiez-vous en recherche d'emploi ?",
    "Aviez-vous refusé une ou plusieurs proposition(s) d'emploi ?",
    "Pour quelle(s) raison(s) aviez-vous refusé ce ou ces emploi(s) ?",
    "Quelle(s) difficulté(s) rencontriez-vous dans votre recherche d'emploi?",
    "Quel type d'études poursuiviez-vous ?",
    "Dans quel établissement étiez-vous inscrit(e) (nom et ville éventuellement) ?",
    "Pour quelle raison aviez-vous principalement choisi de poursuivre des études ?",
    "Vous poursuiviez des études en alternance?",
    "La poursuite d'étude a-t'elle joué un rôle preponderant dans votre insertion professionnelle?",
    "Pays ou territoire du volontariat",
    "Montant brut annuelle HORS primes, de l'allocation perçue?",
    "Montant brut annuelle AVEC primes, de l'allocation perçue?",
    "Nom de l'entreprise qui vous a employé :",
    "Quel type de thèse prépariez-vous ?",
    "Nom et coordonnées de l'entreprise partenaire",
    "Sujet de Thèse",
    "Nom et coordonnées du laboratoire qui vous a accueilli",
    "Le laboratoire est-il localisé?",
    "Région de localisation du laboratoire",	
    "Pays de localisation du laboratoire?",
    "Votre rémunération brute annuelle (hors primes et hors 13ème mois)?",	
    "Perceviez-vous des primes et/ou un 13ème mois?",
    "Votre rémunération brute annuelle AVEC primes et/ou 13ème mois?",
    "La thèse a-t'elle joué un rôle preponderant dans votre insertion professionnelle?",
    "A quel stade de la création/reprise d'entreprise  vous trouviez-vous ?",
    "Année de création /reprise?",
    "Quel est (ou était) le nom de l'entreprise?",
    "Et son activité?",
    "Etiez-vous accompagné par un incubateur et/ou une pépinière d'entreprise?",
    "Lequel ou lesquels?",
    "Quel était le nom de votre entreprise / projet?",
    "Une rapide description de votre activité",
    "Quel etait son secteur d'activité ?",	
    "Le site web s'il existe?",
    "Vos remarques et commentaires relatifs à votre insertion professionnelle",
    "Quels conseils pourriez-vous donner aux étudiants actuellement en formation pour bien choisir leur stage de fin d'étude ? réussir leur insertion professionnelle ?",
    "Votre rémunération brute annuelle AVEC primes et 13ème mois",
    "De quel type de volontariat s'agit-il ?",
    "Durée du contrat de volontariat (en mois)",
    "Montant brut annuel HORS PRIME de l'allocation que vous percevez<br />",
    "Montant brut annuelle AVEC primes, de l'allocation que vous percevez?",
    "Nom de l'entreprise qui vous emploie :",
    "L\'entreprise que vous créez/reprise est-elle déjà en activité, ou à l\'état de projet/reprise plus ou moins avancé ?",
    "année de création/reprise de l'entreprise",
    "Accompagnement par un incubateur et/ou une pépinière d'entreprise?",
    "Une rapide description de son activité",
    "Quel est son secteur d'activité ?",
    "Votre entreprise est/sera localisée?",
    "Région de localisation de la future entreprise",
    "Avez-vous un statut de :",
    "Ce temps partiel est-il voulu ou subi ?",
    "Quelle part de temps partiel réalisez-vous ?",
    "Disposez-vous d'une RQTH (Reconnaissance de la qualité de Travailleur Handicapé)?",
    "Quelle est son adresse?",
    "Votre téléphone professionnel :",
    "Votre courriel professionnel ?",
    "Plus précisement dans la Production et distribution d\'eau assainissement, gestion des déchets et dépollution, il s\'agit de:",
    "Plus précisement dans le domaine du transport, il s'agit :",
    "Plus précisement dans le domaine de l\'élaboration et caractérisation des matériaux, il s\'agit :",
    "Plus précisement dans le domaine de l\' Energie, électronique, électrotechnique, il s\'agit :",
    "Plus précisement dans le domaine du BTP, il s\'agit :",
    "Vous pouvez préciser Divers, il s\'agit :",
    "Positionnez l'activité de votre entreprise dans les domaines suivants :",
    "Positionnez votre propre activité dans les domaines suivants :",
    "Vos activités actuelles font appel à vos compétences en",
    "Pourquoi considérez vous votre emploi comme directement lié aux energies renouvelables?",
    "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Allemand]",
    "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Espagnol]",
    "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Chinois]",
    "Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Autre langue]",
    "Quels ont été les moyens principaux utilisés pour trouver votre emploi actuel?",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La réputation de la filière de formation]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La réputation de l'école/réseau Polytech]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Réseau des anciens (Polytech connect)]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Stage de 4ème année]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Stage de 5ème année]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos compétences linguistiques]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos expériences à l'étranger ]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos expériences à l'école (BDE, GEPI, startup we..)]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La formation]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Votre expérience professionnelle (essentiellement s'il ne s'agit pas du 1er poste)]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos engagements dans des associations et/ou projets ]",
    "Votre entreprise intègre-t-elle une dimension 'Développement durable et responsabilité sociétale' ?",
    "Votre mission intègre-t-elle une dimension 'Développement durable et responsabilité sociétale' ?",
    "Quels sont les aspects du Developpement Durable/Responsabilité sociétale qui vous concernent dans votre mission?",
    "De quel type de volontariat s'agissait-il ?",
    "Durée du contrat de volontariat (en mois).1",
    "Pays ou territoire du volontariat.1",
    "Nom et coordonnées de l'entreprise partenaire.1",
    "Le laboratoire est-il localisé?.1",
    "Région de localisation du laboratoire.1",
    "Pays de localisation du laboratoire?.1",
    "Lequel ou lesquels?.1",
    "Le site web s'il existe?.1",
    "Quelle fut la durée de votre CDD? ",
    "Dans quelle région?.1",
    "Dans quel pays ?",
    "Quelle est son adresse?.1",
    "Ce temps partiel était-il voulu ou subi ?",
    "Quelle part de temps partiel réalisiez-vous ?",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation de l'école/réseau Polytech]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Réseau des anciens (Polytech Connect)]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Stage de 4ème année]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Stage de 5ème année]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos compétences linguistiques]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos experiences à l'étranger ]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos experiences à l'école (BDE, GEPI, startup we..)]",
    "Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La formation]",
    "Globalement,sur une échelle allant de 1 à 5, êtes-vous satisfait(e) de votre formation à Polytech Montpellier ?",
    "L'accompagnement de votre école dans votre projet professionnel vous a-t-il paru?",
    "Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d'apprentissage par l'école ?",
    "Depuis que vous avez quitté l'école, êtes-vous encore en contact avec des enseignants du département?",
    "Quels enseignements vous semblent les plus utiles pour l'exercice de votre métier et votre insertion professionnelle ?",
    "Parmi les enseignements fournis par l'école, quels sont ceux qui mériteraient d'être approfondis ou renforcés ?",
    "Quels enseignements, absents de votre formation, vous auraient été utiles ?",
    "Quels enseignements, présents dans votre formation, vous paraissent inutiles ?",
    "Quels sont vos projets d'évolution de carrière ?",
    "Souhaitez vous être informé sur la nouvelle association des diplômés de Polytech (Polytech Connect)?",
    "Accepteriez-vous d'être sollicité par l'école pour intervenir dans la formation ?",
    "Sous quelle(s) forme(s), désireriez-vous intervenir?"
]

donnees_2018 = donnees_2018.drop(columns=[colonne for colonne in colonnes_a_supprimer if colonne in donnees_2018.columns])

donnees_2018.to_csv('enquete_2018DS-2_nettoyer.csv', index=False) 