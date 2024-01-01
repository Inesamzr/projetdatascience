import pandas as pd

# Charger le fichier CSV
data = pd.read_csv('enquete_2019DS-2.csv', delimiter=';')

print(data.columns)
# Renommer les colonnes

data.rename(columns={'Date': 'date', 'Quel était votre rémunération brut AVEC PRIMES et 13ème mois?' : 'remuneration_annuelle_brute_avec_prime_premier_emploi', 'Quel était votre rémunération brute annuelle (HORS PRIMES et 13ème mois) ?' : 'remuneration_annuelle_brute_hors_prime_premier_emploi','Perceviez vous des primes?':'prime_premier_emploi', 'Quel est le secteur d activité de cette entreprise ?': 'secteur_activite_entreprise_premier_emploi', 'Quel est le secteur d activité de votre entreprise ?':'secteur_activite', 'Avez-vous un statut de cadre ou assimilé ?':'statut_cadre', 'Votre 1er emploi actuel se trouvait-il ?' : 'localisation_premier_emploi','Dans quelle région?.1' : 'region_premier_emploi', 'Quel est le domaine général de votre entreprise ?':'domaine_entreprise','Quel est votre salaire brut ANNUEL AVEC PRIMES et/ou 13ème mois ?': 'remuneration_prime', 'Percevez vous des primes et/ou un 13ème mois?':'prime','Quel est l intitulé de votre poste ?': 'intitule_poste', 'Sexe': 'sexe', 'Année obtention du diplôme': 'date_diplome', 'Formation': 'filiere', 'Vous avez la nationalité française?': 'nationalite_francaise', 'Quelle est votre nationalité?': 'nationalite', 'Pour votre dernière année, vous etiez inscrit(e) en :': 'type_formation', 'Quelle est votre situation actuelle ?' : 'situation_actuelle', 'Êtes-vous à la recherche d un emploi depuis la sortie de l école ? ': 'en_recherche_emploi', 'Depuis combien de mois êtes-vous en recherche d emploi ? ': 'nb_mois_recherche_emploi', 'Avez-vous refusé une ou plusieurs proposition(s) d emploi depuis ?': 'refus_emploi','Quel est votre salaire BRUT ANNUEL HORS PRIMES et HORS 13ème mois ?':'remuneration_annuelle_brute', 'Quelle(s) difficulté(s) rencontrez-vous dans votre recherche d emploi?': 'difficultés_recherche', 'vous poursuivez des études en alternance?' : 'alternance', 'Quel est le poste que vous occupez?': 'poste_occupe', 'Quel est son secteur d activité ?': 'secteur_activite', 'Avez-vous eu une autre activité (emploi CDD/CDI, poursuite d étude, thèse, création d entreprise) entre votre sortie de l école et votre situation actuelle ?': 'autre_activite', 'Sur cet emploi, vous êtes :': 'statut_emploi', 'Quelle est la nature de votre contrat de travail ?': 'nature_contrat', 'Quelle est la durée de votre CDD?': 'duree_CDD', 'Votre emploi actuel se trouve ?': 'localisation_emploi', 'Dans quelle région?': 'region_emploi', 'Dans quel pays?': 'pays_localisation', 'Travaillez-vous à temps partiel ?': 'temps_partiel', 'Depuis combien de mois occupez-vous cet emploi ?': 'nb_mois_occupation_emploi', 'Disposez-vous d une RQTH (Reconnaissance de la qualité de Travailleur Handicapé)?': 'handicap', 'Quelle est la taille de l entreprise (ou du groupe si l entreprise appartient à un groupe) dans laquelle (ou lequel) vous travaillez ?': 'taille_entreprise', 'Dans quel service ou département exercez-vous cet emploi (à défaut de service ou de département, indiquez votre fonction principale) ?': 'service_emploi', 'Vous poursuiviez des études en alternance?' : 'alternance', 'Aviez-vous un statut de :' : 'statut', '"Sur ce 1er emploi vous étiez :"': 'statut_premier_emploi', 'Quelle etait la nature de votre contrat de travail ?': 'nature_contrat_premier_emploi', 'Quel était l intitulé du poste occupé ?':'intitule_premier_emploi', 'Dans quel pays ?' : 'pays_premier_emploi', 'Commune de l entreprise.1': 'ville_premier_emploi', 'Quelle est la taille de l entreprise (ou du groupe si l entreprise appartient à un groupe) dans laquelle (ou lequel) vous travailliez ?': 'taille_entreprise_premier_emploi', 'Aviez-vous un statut de cadre dans ce premier emploi ?': 'statut_cadre_premier_emploi','Travailliez-vous à temps partiel ?': 'temps_partiel_premier_emploi'}, inplace = True)

columns_to_drop = ['Champ 1','Comment jugeriez-vous les aspects suivants de votre emploi ? [Votre niveau de rémunération]','Comment avez-vous trouvé votre emploi actuel?','Responsabilités exercées [Vous avez des personnes sous votre responsabilité hiérarchique ?]','Positionnez l activité de votre entreprise dans les domaines suivants :','Adresse ( numero rue avenue..)de l entreprise','Responsabilités exercées [Vous avez la responsabilité d une équipe ?]', 'Pour quelle(s) raison(s) aviez-vous refusé ce ou ces emploi(s) ?', 'Champ 2', 'Vous occupez plusieurs emplois?', 'Quel type d études poursuivez-vous ?', 'Concours préparé(s)', 'Dans quel établissement êtes-vous inscrit(e) (nom et ville éventuellement) ?', 'Pour quelle raison avez-vous principalement choisi de poursuivre des études ?', 'Le laboratoire est-il localisé?', 'Vous bénéficiez d une source de financement contractuel, d une allocation ou d un contrat doctoral','Quel est le type de financement contractuel qui permet de financer votre thèse ?', 'De quel type de volontariat s agit-il ?', 'Durée du contrat de volontariat (en mois)','Pays ou territoire du volontariat', 'Montant brut annuel HORS PRIME de l allocation que vous percevez<br />', 'Montant brut annuelle AVEC primes, de l allocation que vous percevez?',  'Nom de l entreprise qui vous emploie :', 'l entreprise que vous créez/reprise est-elle déjà en activité, ou à l état de projet/reprise plus ou moins avancé ?', 'année de création/reprise de l entreprise','Accompagnement par un incubateur et/ou une pépinière d entreprise?', 'Lequel ou lesquels?', 'Quel est le nom de votre entreprise / projet?','Une rapide description de son activité','Votre entreprise est/sera localisée?', 'Le site web s il existe?', 'Avez-vous un statut de :', 'Pays de localisation de la future entreprise?', 'Région de localisation de la future entreprise', 'Ce temps partiel est-il voulu ou subi ?', 'Quelle part de temps partiel réalisez-vous ?', 'Quel est le nom de l entreprise dans laquelle vous travaillez ?', 'Adresse ( numero, rue, avenue..)de l entreprise', 'Code postal de l entreprise', 'Commune de l entreprise', ' Son site internet?', 'Lequel?', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation du département]', 'Globalement vous êtes satisfait(e) de votre premier emploi?', 'l accompagnement de votre école dans votre projet professionnel vous a-t-il paru?', 'Quels enseignements vous semblent les plus utiles pour l exercice de votre métier et votre insertion professionnelle ?', 'Parmi les enseignements fournis par l école, quels sont ceux qui mériteraient d être approfondis ou renforcés ?', 'Quels enseignements, absents de votre formation, vous auraient été utiles ?', 'Quels enseignements, présents dans votre formation, vous paraissent inutiles ?', 'Votre double compétence en Informatique & Gestion vous semble-t-elle  constituer un plus pour votre insertion professionnelle ? ', 'Pour quelle(s) raison(s) avez-vous refusé ce ou ces emploi(s) ?','Sans activité volontairement, merci de préciser :', 'Etes-vous membre de réseaux ou associations  professionnelles liées à l informatique  ? ', 'Laquelle?', 'Quels sont vos projets d évolution de carrière ?', 'Souhaitez vous être informé sur l  association des diplômés de Polytech (Polytech Connect)?', 'Responsabilités exercées [ Vous avez la responsabilité d un projet ?]', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Français]',  'Souhaitez vous être informé sur l  association des diplômés de Polytech (Polytech Connect)?', 'Responsabilités exercées [ Vous avez la responsabilité d un projet ?]', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Français]', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Allemand]', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Chinois]', 'Votre mission intègre-t-elle une dimension Développement durable et responsabilité sociétale ?','Quels sont les aspects du Developpement Durable/Responsabilité sociétale qui vous concernent dans votre mission?','Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage domaine Eau et Génie Civil]','Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage Mécanique structures industrielles chaudronnées]', 'Accepteriez-vous d être sollicité par l école pour intervenir dans la formation ?','Comment jugeriez-vous les aspects suivants de votre emploi ? [Vos conditions de travail]', 'Pourquoi considérez vous votre emploi comme directement lié aux energies renouvelables?', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Anglais]', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Espagnol]', 'Sous quelle(s) forme(s), désireriez-vous intervenir?', 'l entreprise dans laquelle vous travaillez actuellement appartient-elle à un groupe ?','Quels sont les langages informatiques que vous pratiquez dans le cadre de votre activité :', 'Positionnez votre propre activité dans les domaines suivants :', 'Depuis que vous avez quitté l école êtes-vous encore en contact avec des enseignants du département?', 'Plus précisement dans le domaine de l  Energie électronique électrotechnique il s agit :', 'Plus précisement dans le domaine du BTP il s agit :', 'Plus précisement dans la Production et distribution d eau assainissement gestion des déchets et dépollution il s agit de ', 'Recommanderiez-vous Polytech Montpellier à un(e) ami(e) qui souhaite poursuivre un cursus dans l enseignement supérieur ?', 'Votre fonction est-elle liée à l international ? ', 'Utilisez-vous fréquemment les langues suivantes dans votre travail ? [Autre langue]', 'Quels conseils pourriez-vous donner aux étudiants actuellement en formation pour bien choisir leur stage de fin d étude ? réussir leur insertion professionnelle ?', 'Plus précisement dans le domaine du transport il s agit :','Vos activités actuelles font appel à vos compétences en','Considérez vous que votre emploi est directement lié aux énergies renouvelables ?','Responsabilités exercées [Vous avez la responsabilité d un budget ?]','Vous pouvez préciser Divers il s agit :', 'Plus précisement dans le domaine de l élaboration et caractérisation des matériaux il s agit :', 'Combien de temps avez-vous mis pour trouver votre emploi actuel?', 'Quels ont été les moyens principaux utilisés pour trouver votre emploi actuel?','Quel a été votre principal critère de choix ?' ,'Estimez-vous que votre emploi correspond [A votre niveau de qualification ?]','Estimez-vous que votre emploi correspond [Au secteur disciplinaire de votre formation ?]', 'Globalement êtes-vous satisfait(e) de votre emploi actuel ?',        'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Réseau des anciens (Polytech connect)]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Stage de 4ème année]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Stage de 5ème année /Contrat de professionnalisation]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos compétences linguistiques]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos expériences à l étranger ]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos expériences à l école (BDE GEPI startup we)]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La formation]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Votre expérience professionnelle (essentiellement s il ne s agit pas du 1er poste)]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [Vos engagements dans des associations et/ou projets ]','Vous recherchez un autre emploi?','Pour quelle(s) raison(s) recherchez vous un autre emploi? ','Votre entreprise intègre-t-elle une dimension Développement durable et responsabilité sociétale ?', 'Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [apprentissage Systèmes embarqués]','Votre entreprise pourrait-elle être interessée par un ou des étudiants en: [contrat de professionnalisation]','Quelle était votre situation pour cette 1ère activité qui a fait suite à la sortie de Polytech ?','Depuis combien de temps étiez-vous en recherche d emploi ?','Aviez-vous refusé une ou plusieurs proposition(s) d emploi ?','Quelle(s) difficulté(s) rencontriez-vous dans votre recherche d emploi?', 'Quel type d études poursuiviez-vous ?','Concours préparé(s).1','Dans quel établissement étiez-vous inscrit(e) (nom et ville éventuellement) ?','Pour quelle raison aviez-vous principalement choisi de poursuivre des études ?','La poursuite d étude a-t elle joué un rôle preponderant dans votre insertion professionnelle?','De quel type de volontariat s agissait-il ?','Durée du contrat de volontariat (en mois).1','Pays ou territoire du volontariat.1','Montant brut annuelle HORS primes de l allocation perçue?', 'Montant brut annuelle AVEC primes, de l allocation perçue?','Nom de l entreprise qui vous a employé :','Quel type de thèse prépariez-vous ?','Nom et coordonnées de l entreprise partenaire.1','Sujet de Thèse','Nom et coordonnées du laboratoire qui vous a accueilli','Le laboratoire est-il localisé?.1','Région de localisation du laboratoire.1','Pays de localisation du laboratoire?.1', 'La thèse a-t elle joué un rôle preponderant dans votre insertion professionnelle?','A quel stade de la création/reprise d entreprise  vous trouviez-vous ?','Année de création /reprise?','Quel est (ou était) le nom de l entreprise?','Et son activité?','Etiez-vous accompagné par un incubateur et/ou une pépinière d entreprise?','Lequel ou lesquels?.1','Quel était le nom de votre entreprise / projet?','Une rapide description de votre activité','Quel etait son secteur d activité ?','Le site web s il existe?.1', 'Combien de mois avez-vous mis pour trouver votre premier emploi depuis l obtention de votre diplôme ?','Quelle fut la durée de votre CDD? ','Dans quelles conditions s est déroulé votre départ de votre première entreprise ?','Quel est le nom de l entreprise dans laquelle vous travailliez ?', 'Code postal de l entreprise.1','l entreprise dans laquelle vous travailliez appartient-elle à un groupe ?','Lequel?.1','Combien de temps avez vous travaillé dans cette entreprise ?(en mois)','Ce temps partiel était-il voulu ou subi ?','Quelle part de temps partiel réalisiez-vous ?','Comment avez-vous trouvé ce premier emploi ?','Quel fut votre principal critère de choix sur ce 1er emploi?','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La réputation de l école/réseau Polytech]',  'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Réseau des anciens (Polytech Connect)]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Stage de 4ème année]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Stage de 5ème année/Contrat de professionnalisation]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos compétences linguistiques]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos experiences à l étranger ]', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [La formation]','Globalement sur une échelle allant de 1 à 5 êtes-vous satisfait(e) de votre formation à Polytech Montpellier ?', 'Comment jugeriez-vous les aspects suivants de votre emploi ? [Vos relations avec vos collègues]','Comment jugeriez-vous les aspects suivants de votre emploi ? [Votre niveau d autonomie et de responsabilité]','Comment jugeriez-vous les aspects suivants de votre emploi ? [La localisation géographique de votre emploi]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La réputation de la filière de formation]','Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement? [La réputation de l école/réseau Polytech]', 'Sur ce 1er emploi vous étiez :', 'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi? [Vos experiences à l école (BDE GEPI startup we)]','Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d apprentissage par l école ?','Vos remarques et commentaires relatifs à votre insertion professionnelle', 'Quel type de thèse préparez-vous ?', 'Nom et coordonnées de l entreprise partenaire', 'Sujet de votre Thèse', 'Nom et coordonnées du laboratoire qui vous accueille', 'Région de localisation du laboratoire', 'Pays de localisation du laboratoire?', 'Percevez des primes et/ou un 13ème mois?', 'Votre rémunération brute annuelle AVEC primes et 13ème mois', 'Votre rémunération brute annuelle (hors primes et 13ème mois)?',  'Votre rémunération brute annuelle (hors primes et hors 13ème mois)?', 'Perceviez-vous des primes et/ou un 13ème mois?', 'Votre rémunération brute annuelle AVEC primes et/ou 13ème mois?' ]
data.drop(columns=columns_to_drop, inplace=True)

# Afficher les premières lignes pour vérifier la structure des données
print(data.head())

data.to_csv('2019.csv', index=False)

#python3 netoyer.py 