import pandas as pd

# Remplacez par le chemin de votre fichier
data_drop_2023 = pd.read_csv('enquete 2023DS-2.csv', delimiter=';')

# print("Affichage des 5 premières lignes", data_2023.head())

print("Affichage de nombre de lignes et de colonnes", data_drop_2023.shape)
colonnes = data_drop_2023.columns
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
                        'Lequel ou lesquels ?',
                        'Le site web s\'il existe ?',
                        'Disposez-vous d\'une RQTH (Reconnaissance de la qualité de Travailleur Handicapé)?',
                        'Adresse ( numero, rue, avenue..) de l\'entreprise',
                        'Code postal de l\'entreprise',
                        'Son site internet ?',
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
                        'Globalement, êtes-vous satisfait·e de votre emploi actuel ?',
                        'Vous recherchez un autre emploi ?',
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
                        'A quel stade de la création/reprise d\'entreprise vous trouviez-vous ?',
                        'Etiez-vous accompagné·e par un incubateur et/ou une pépinière d\'entreprises ?',
                        'Lequel ou lesquels ?10',
                        'Une rapide description de votre activité;Quel etait son secteur d\'activité ?',
                        'Le site web s\'il existe ?11',
                        'Dans quelles conditions s\'est déroulé votre départ de votre première entreprise ?',
                        'Adresse ( numero, rue, avenue..)de l\'entreprise',
                        'Comment avez-vous trouvé ce premier emploi ?',
                        'Quel fut votre principal critère de choix sur ce 1er emploi ?',
                        'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La réputation du département',
                        'Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La formation',
                        'Globalement, sur une échelle allant de 1 à 5, êtes-vous satisfait·e de votre formation à Polytech Montpellier ?',
                        'L\'accompagnement de votre école dans votre projet professionnel vous a-t-il paru...',
                        'Etes-vous membre de réseaux ou associations  professionnelles liées à l\'informatique  ?',
                        'Laquelle ?',
                        'Quels sont vos projets d\'évolution de carrière ?',
                        'Accepteriez-vous d\'être sollicité·e par l\'école pour intervenir dans la formation ?',
                        'Sous quelle(s) forme(s), désireriez-vous intervenir ?',
                        'Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d\'apprentissage par l\'école ?',
                        'Depuis que vous avez quitté l\'école, êtes-vous encore en contact avec des enseignants du département ?',
                        'Recommanderiez-vous Polytech Montpellier à un·e ami·e qui souhaite poursuivre un cursus dans l\'enseignement supérieur ?'

                        ]
data_drop_2023.drop(columns=colonnes_a_supprimer, inplace=True)





Date;Genre;identifiant;Vous avez la nationalité française?;Quelle est votre nationalité?;
Année obtention du diplôme;Pour votre dernière année, vous étiez inscrit·e en :;
Région de votre entreprise d'accueil;Durant vos études, vous avez bénéficié d'aménagements de scolarité liés à un handicap, ou vous vous êtes déclaré·e en situation d'handicap auprès de votre établissement;Formation;Quelle est votre situation actuelle ?;'Sans activité volontairement', merci de préciser :;Vous occupez plusieurs emplois?;Êtes-vous à la recherche d'un emploi depuis la sortie de l'école ? ;Depuis combien de mois êtes-vous en recherche d'emploi ? ;
Avez-vous refusé une ou plusieurs proposition(s) d'emploi depuis ?;Pour quelle(s) raison(s) avez-vous refusé ce ou ces emploi(s) ?;Quelle(s) difficulté(s) rencontrez-vous dans votre recherche d'emploi?;Quel type d'études poursuivez-vous ?;Concours préparé(s);Dans quel établissement êtes-vous inscrit·e (nom et ville éventuellement) ?;Vous poursuivez des études en alternance?;Pour quelle raison avez-vous principalement choisi de poursuivre des études ?;Quel type de thèse préparez-vous ?;Nature de votre contrat de travail ?;Nom et coordonnées de l'entreprise partenaire;Sujet de votre Thèse / PhD;Nom et coordonnées du laboratoire qui vous accueille;Le laboratoire est-il localisé?;Région de localisation du laboratoire;Pays de localisation du laboratoire?;Vous bénéficiez d'une source de financement contractuel, d'une allocation ou d'un contrat doctoral ?;Quel est le type de financement contractuel qui permet de financer votre thèse ?;Votre rémunération brute annuelle (hors primes )?;Vous percevez des primes ?;Votre rémunération brute annuelle AVEC primes;De quel type de volontariat s'agit-il ?;Durée du contrat de volontariat (en mois) ;Pays ou territoire du volontariat;Quel est le montant brut annuel de l'allocation que vous percevez ?<br />;Quel est le poste que vous occupez ?;Quel est le statut de votre employeur / entreprise ? ;Nom de l'entreprise qui vous emploie :;Quel est son secteur d'activité ?;L'entreprise que vous créez/reprise est-elle déjà en activité, ou à l'état de projet/reprise plus ou moins avancé ?;Année de création/reprise de l'entreprise;Accompagnement par un incubateur et/ou une pépinière d'entreprise ?;Lequel ou lesquels ?;Quel est le nom de votre entreprise / projet ?;Une rapide description de son activité;Quel est son secteur d'activité ?4;Votre entreprise est/sera localisée...;Région de localisation de la future entreprise;Pays de localisation de la future entreprise ?;Le site web s'il existe ?;Avez-vous eu une autre activité (emploi CDD/CDI, poursuite d'étude, thèse, création d'entreprise) entre votre sortie de l'école et votre situation actuelle ?;Sur cet emploi, vous êtes :;'Non salarié·e·s, merci de préciser :;Quelle est la nature de votre contrat de travail ?;Quelle est la durée de votre CDD ?;Quelle est la date de votre embauche ?;Votre emploi actuel se trouve ?;Dans quelle région ?;Dans quel pays ?;Vous travaillez;Ce temps partiel est-il voulu ou subi ?;Quelle part de temps partiel réalisez-vous ?;Depuis combien de mois occupez-vous cet emploi ?;Combien de jours par semaine êtes-vous en télétravail ? ;Disposez-vous d'une RQTH (Reconnaissance de la qualité de Travailleur Handicapé)?;Quel est le statut de votre employeur / entreprise ?;Quelle est la raison sociale de votre employeur / votre entreprise ?;Adresse ( numero, rue, avenue..) de l'entreprise;Code postal de l'entreprise;Commune de l'entreprise; Son site internet ?;Nombre de salarié·e·s de votre employeur / entreprise;L'entreprise dans laquelle vous travaillez actuellement appartient-elle à un groupe ?;Lequel?;Votre employeur / Votre entreprise est une ESN (Entreprise de Services du Numérique) - anciennement SSII ?;Votre employeur / Votre entreprise est une société de conseil ou d'ingénierie, un bureau d'études indépendant ?;Votre employeur / Votre entreprise est une société d'intérim pour laquelle vous exercez une mission ?;Dans quel service ou département exercez-vous cet emploi (à défaut de service ou de département, indiquez votre fonction principale) ?;Quel est l'intitulé de votre poste ?;Avez-vous un statut de cadre ou assimilé ?;Quel est le secteur d'activité de votre entreprise (celle qui vous rémunère)?;Quel est le secteur dans lequel vous intervenez principalement ?;Considérez vous que votre emploi est directement lié aux énergies renouvelables ?;Pourquoi considérez vous votre emploi comme directement lié aux energies renouvelables?;Les enjeux de la RSE - Responsabilité Sociétale des Entreprises - font partie de votre mission;Quels sont les aspects du Developpement Durable/Responsabilité sociétale qui vous concernent dans votre mission?;Votre diplôme vous a permis d'acquérir les compétences demandées par votre employeur en matière de transformations environnementales?;Les enjeux de la transition écologique font partie des préoccupations de votre employeur / votre entreprise ?;Quel est votre salaire BRUT ANNUEL (HORS PRIMES)?;Percevez vous des primes et/ou un 13ème mois ?;Quel est votre salaire brut ANNUEL AVEC PRIMES ?;Responsabilités exercées - Vous avez des personnes sous votre responsabilité hiérarchique ?;Responsabilités exercées - Vous avez la responsabilité d'un budget ?;Responsabilités exercées - Vous avez la responsabilité fonctionnelle  d'une équipe ?;Responsabilités exercées -  Vous avez la responsabilité d'un projet ?;Votre fonction est-elle liée à l'international ? ;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Français;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Anglais;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Allemand;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Espagnol;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Chinois;Utilisez-vous fréquemment les langues suivantes dans votre travail ? - Autre langue;Combien de temps avez-vous mis pour trouver votre emploi actuel ?;Comment avez-vous trouvé votre emploi actuel ?;Quels ont été les moyens principaux utilisés pour chercher votre emploi actuel ?;Vos principaux critères de choix ?;Estimez-vous que votre emploi correspond... - A votre niveau de qualification ?;Estimez-vous que votre emploi correspond... - Au secteur disciplinaire de votre formation ?;Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos conditions de travail;Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos relations avec vos collègues;Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau de rémunération;Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau d'autonomie et de responsabilité;Comment jugeriez-vous les aspects suivants de votre emploi ? - La localisation géographique de votre emploi;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - La réputation de la filière de formation;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - La formation;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - La réputation de l'école/réseau Polytech;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Réseau des anciens (Polytech connect);Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Stage de 4ème année;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Stage de 5ème année /Contrat de professionnalisation;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Vos compétences linguistiques;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Vos expériences à l'étranger ;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Vos expériences à l'école (BDE, GEPI, startup we..);Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Votre expérience professionnelle (essentiellement s'il ne s'agit pas du 1er poste);Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement ? - Vos engagements dans des associations et/ou projets ;Globalement, êtes-vous satisfait·e de votre emploi actuel ?;Vous recherchez un autre emploi ?;Pour quelle(s) raison(s) recherchez vous un autre emploi ? ;Quelle était votre situation pour cette 1ère activité qui a fait suite à la sortie de Polytech ?;Depuis combien de temps étiez-vous en recherche d'emploi ?;Aviez-vous refusé une ou plusieurs proposition(s) d'emploi ?;Pour quelle(s) raison(s) aviez-vous refusé ce ou ces emploi(s) ?;Quelle(s) difficulté(s) rencontriez-vous dans votre recherche d'emploi ?;Quel type d'études poursuiviez-vous ?;Concours préparé(s)6;Dans quel établissement étiez-vous inscrit·e (nom et ville éventuellement) ?;Pour quelle raison aviez-vous principalement choisi de poursuivre des études ?;Vous poursuiviez des études en alternance ?;La poursuite d'étude a-t'elle joué un rôle preponderant dans votre insertion professionnelle ?;De quel type de volontariat s'agissait-il ?;Durée du contrat de volontariat (en mois);Pays ou territoire du volontariat7;Montant brut annuelle HORS primes, de l'allocation perçue ?;Montant brut annuelle AVEC primes, de l'allocation perçue ?;Nom de l'entreprise qui vous a employé :;Quel type de thèse prépariez-vous ?;Nom et coordonnées de l'entreprise partenaire8;Sujet de Thèse;Nom et coordonnées du laboratoire qui vous a accueilli;Le laboratoire est localisé...;Région de localisation du laboratoire9;Pays de localisation du laboratoire ?;Votre rémunération brute annuelle (hors primes et hors 13ème mois)?;Perceviez-vous des primes et/ou un 13ème mois ?;Votre rémunération brute annuelle AVEC primes et/ou 13ème mois ?;La thèse a-t-elle joué un rôle prépondérant dans votre insertion professionnelle ?;A quel stade de la création/reprise d'entreprise  vous trouviez-vous ?;Année de création/reprise ?;Quel est (ou était) le nom de l'entreprise ?;Et son activité ?;Etiez-vous accompagné·e par un incubateur et/ou une pépinière d'entreprises ?;Lequel ou lesquels ?10;Quel était le nom de votre entreprise / projet ?;Une rapide description de votre activité;Quel etait son secteur d'activité ?;Le site web s'il existe ?11;Combien de mois avez-vous mis pour trouver votre premier emploi depuis l'obtention de votre diplôme ?;Sur ce 1er emploi, vous étiez :;Aviez-vous un statut de :;Quelle etait la nature de votre contrat de travail ?;Quelle fut la durée de votre CDD? ;Dans quelles conditions s'est déroulé votre départ de votre première entreprise ?;Quel était l'intitulé du poste occupé ?;Votre 1er emploi se trouvait-il...;Dans quelle région ?12;Dans quel pays ?13;Quel est le nom de l'entreprise dans laquelle vous travailliez ?;Adresse ( numero, rue, avenue..)de l'entreprise;Code postal de l'entreprise14;Commune de l'entreprise15;Quelle est la taille de l'entreprise (ou du groupe si l'entreprise appartient à un groupe) dans laquelle (ou lequel) vous travailliez ?;L'entreprise dans laquelle vous travailliez appartient-elle à un groupe ?;Lequel?16;Quel est le secteur d'activité de cette entreprise ?;Combien de temps avez vous travaillé dans cette entreprise ? (en mois);Aviez-vous un statut de cadre dans ce premier emploi ?;Travailliez-vous à temps partiel ?;Ce temps partiel était-il voulu ou subi ?;Quelle part de temps partiel réalisiez-vous ?;Comment avez-vous trouvé ce premier emploi ?;Quel fut votre principal critère de choix sur ce 1er emploi ?;Quel était votre rémunération brute annuelle (HORS PRIMES) ?;Perceviez-vous des primes ?;Quel était votre rémunération brut AVEC PRIMES ?;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La réputation du département;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La formation;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - La réputation de l'école/réseau Polytech;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Réseau des anciens (Polytech Connect);Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Stage de 4ème année;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Stage de 5ème année/Contrat de professionnalisation;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Vos compétences linguistiques;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Vos experiences à l'étranger ;Les éléments suivants vous semblent-ils avoir joué un rôle dans votre recrutement sur ce 1er emploi ? - Vos experiences à l'école (BDE, GEPI, startup we..);Globalement, vous êtes satisfait·e de votre premier emploi ?;La situation sanitaire a-t-elle eu un impact pour vous ?;Merci de préciser les raisons;Globalement, sur une échelle allant de 1 à 5, êtes-vous satisfait·e de votre formation à Polytech Montpellier ? ;L'accompagnement de votre école dans votre projet professionnel vous a-t-il paru...;Quels enseignements vous semblent les plus utiles pour l'exercice de votre métier et votre insertion professionnelle ?;Parmi les enseignements fournis par l'école, quels sont ceux qui mériteraient d'être approfondis ou renforcés ?;Quels enseignements, absents de votre formation, vous auraient été utiles ?;Quels enseignements, présents dans votre formation, vous paraissent inutiles ?;Votre double compétence en Informatique & Gestion vous semble-t-elle  constituer un plus pour votre insertion professionnelle ? ;Etes-vous membre de réseaux ou associations  professionnelles liées à l'informatique  ? ;Laquelle ?;Quels sont vos projets d'évolution de carrière ?;Accepteriez-vous d'être sollicité·e par l'école pour intervenir dans la formation ?;Sous quelle(s) forme(s), désireriez-vous intervenir ?;Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d'apprentissage par l'école ?;Depuis que vous avez quitté l'école, êtes-vous encore en contact avec des enseignants du département ?;Recommanderiez-vous Polytech Montpellier à un·e ami·e qui souhaite poursuivre un cursus dans l'enseignement supérieur ?;Quels conseils pourriez-vous donner aux étudiants actuellement en formation pour bien choisir leur stage de fin d'étude ? réussir leur insertion professionnelle ?;Vos remarques et commentaires relatifs à votre insertion professionnelle