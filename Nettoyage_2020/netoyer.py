import pandas as pd

# Charger le fichier CSV
data = pd.read_csv('enquete_2020DS.csv', delimiter=';')

# Afficher les premières lignes pour vérifier la structure des données
print(data.head())

# Renommer les colonnes
data.rename(columns={'Année obtention du diplôme': 'date_diplome', 
'Formation': 'filiere', 
'Vous avez la nationalitÈ franÁaise?': 'nationalite_francaise', 
'AnnÈe obtention du diplÙme' : 'date_diplome',
'Quelle est votre nationalitÈ?': 'nationalite',
'Pour votre derniËre annÈe, vous etiez inscrit(e) en :' : 'type_formation',
'Quelle est votre situation actuelle ?' : 'situation_actuelle',
'tes-vous ‡ la recherche d un emploi depuis la sortie de l Ècole ? ' : 'en_recherche_emploi',
'Depuis combien de mois Ítes-vous en recherche d emploi ?' : 'nb_mois_recherche_emploi',
'Quelle(s) difficultÈ(s) rencontrez-vous dans votre recherche d emploi?' : 'difficultés_recherche',
'Vous poursuiviez des Ètudes en alternance?' : 'alternance',

'Avez-vous eu une autre activitÈ (emploi CDD/CDI, poursuite d Ètude, thËse, crÈation d entreprise) entre votre sortie de l Ècole et votre situation actuelle ?' : 'autre_activite',
'Sur cet emploi, vous Ítes :' : 'statut_emploi',
'Quelle est la nature de votre contrat de travail ?' : 'nature_contrat',
'Quelle est la durÈe de votre CDD?' : 'duree_CDD',
'Votre emploi actuel se trouve ?' : 'localisation_emploi',
'Dans quelle rÈgion?' : 'region_emploi',
'Dans quel pays?' : 'pays_emploi',
'Travaillez-vous ‡ temps partiel ?' : 'temps_partiel',
'Ce temps partiel est-il voulu ou subi ?' : 'temps_partiel_voulu',
'Depuis combien de mois occupez-vous cet emploi ?' : 'mois_emploi_occupe',
'Quel est le nom de l entreprise dans laquelle vous travaillez ?' : 'nom_entreprise',
'Quelle est la taille de l entreprise (ou du groupe si l entreprise appartient ‡ un groupe) dans laquelle (ou lequel) vous travaillez ?' : 'taille_entreprise',
'L entreprise dans laquelle vous travaillez actuellement appartient-elle ‡ un groupe ?' : 'entreprise_groupe',
'Dans quel service ou dÈpartement exercez-vous cet emploi (‡ dÈfaut de service ou de dÈpartement, indiquez votre fonction principale) ?' : 'fonction_principale',
'Quel est l intitulÈ de votre poste ?' : 'intitule_poste',
'Avez-vous un statut de cadre ou assimilÈ ?' : 'statut_cadre',
'Quel est le secteur d activitÈ de votre entreprise ?' : 'secteur_activite_entreprise',
'Quel est le domaine gÈnÈral de votre entreprise ?' : 'domaine_entreprise',
'Vous pouvez prÈciser Divers, il s agit :' : 'domaine_divers',
'Quels sont les langages informatiques que vous pratiquez dans le cadre de votre activitÈ :' : 'langage_info',
'ConsidÈrez vous que votre emploi est directement liÈ aux Ènergies renouvelables ?' : 'energie_renouvelable',
'Quel est votre salaire BRUT ANNUEL HORS PRIMES et HORS 13Ëme mois ?' : 'remuneration_annuelle_brute',
'Percevez vous des primes et/ou un 13Ëme mois?' : 'prime',
'Quel est votre salaire brut ANNUEL AVEC PRIMES et/ou 13Ëme mois ?' : 'remuneration_prime',
'ResponsabilitÈs exercÈes - Vous avez des personnes sous votre responsabilitÈ hiÈrarchique ?' : 'responsabilite_hierarchie',
'ResponsabilitÈs exercÈes - Vous avez la responsabilitÈ d un budget ?' : 'responsabilite_budget',
'ResponsabilitÈs exercÈes - Vous avez la responsabilitÈ d une Èquipe ?' : 'responsabilite_equipe',
'ResponsabilitÈs exercÈes -  Vous avez la responsabilitÈ d un projet ?' : 'responsabilite_projet',
'Votre fonction est-elle liÈe ‡ l international ? ' : 'fonction_international',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - FranÁais' : 'langue_francais',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - Anglais' :'langue_anglais',
'Combien de temps avez-vous mis pour trouver votre emploi actuel?' : 'temps_trouve_emploi',
'Comment avez-vous trouvÈ votre emploi actuel?' : 'moyen_trouver_emploi',
'Quel a ÈtÈ votre principal critËre de choix ?' : 'critere_choix_emploi',
'Estimez-vous que votre emploi correspond - A votre niveau de qualification ?' : 'emploi_niveau_qualification',
'Estimez-vous que votre emploi correspond - Au secteur disciplinaire de votre formation ?' : 'emploi_formation',
'Globalement, Ítes-vous satisfait(e) de votre emploi actuel ?' : 'satisfaction_emploi',
'Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos conditions de travail' : 'condition_travail',
'Comment jugeriez-vous les aspects suivants de votre emploi ? - Vos relations avec vos collËgues' : 'relation_collegues',
'Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau de rÈmunÈration' : 'niveau_remuneration',
'Comment jugeriez-vous les aspects suivants de votre emploi ? - Votre niveau d autonomie et de responsabilitÈ' : 'niveau_autonomie',
'Comment jugeriez-vous les aspects suivants de votre emploi ? - La localisation gÈographique de votre emploi' : 'jugement_localisation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - La rÈputation de la filiËre de formation'
'Vous recherchez un autre emploi?' : 'recherche_nouvel_emploi',
'Pour quelle(s) raison(s) recherchez vous un autre emploi? ' : 'raisons_recherche_emploi',
'Votre entreprise intËgre-t-elle une dimension DÈveloppement durable et responsabilitÈ sociÈtale ?' : 'entreprise_developpement_durable',
'Votre mission intËgre-t-elle une dimension DÈveloppement durable et responsabilitÈ sociÈtale ?' : 'mission_developpement_durable',
'Quelle Ètait votre situation pour cette 1Ëre activitÈ qui a fait suite ‡ la sortie de Polytech ?' : 'situation_sortie_polytech',
'Aviez-vous refusÈ une ou plusieurs proposition(s) d emploi ?' : 'refus_emploi',
'Combien de mois avez-vous mis pour trouver votre premier emploi depuis l obtention de votre diplÙme ?' : 'temps_trouve_premier_emploi',
'Sur ce 1er emploi, vous Ètiez :' : 'post_premier_emploi',
'Quelle etait la nature de votre contrat de travail ?' : 'nature_contrat_premier_emploi',
'Quelle fut la durÈe de votre CDD?' : 'duree_cdd_premier_emploi',
'Dans quelles conditions s est dÈroulÈ votre dÈpart de votre premiËre entreprise ?' : 'condition_fin_premier_contrat',
'Quel Ètait l intitulÈ du poste occupÈ ?' : 'intitule_premier_emploi',
'Votre 1er emploi actuel se trouvait-il ?' : 'localisation_premier_emploi',
'Dans quelle rÈgion?.1' : 'region_premier_emploi',
'Dans quel pays ?' : 'pays_premier_emploi',
'Quel est le nom de l entreprise dans laquelle vous travailliez ?' : 'nom_entreprise_premier_emploi',
'Quelle est la taille de l entreprise (ou du groupe si l entreprise appartient ‡ un groupe) dans laquelle (ou lequel) vous travailliez ?' : 'taille_entreprise_premier_emploi',
'L entreprise dans laquelle vous travailliez appartient-elle ‡ un groupe ?' : 'premiere_entreprise_groupe',
'Lequel?' : 'groupe',
'Quel est le secteur d activitÈ de cette entreprise ?' : 'secteur_premiere_entreprise',
'Combien de temps avez vous travaillÈ dans cette entreprise ?(en mois)' : 'anciennete_premier_emploi',
'Aviez-vous un statut de cadre dans ce premier emploi ?' : 'status_cadre_premier_emploi',
'Travailliez-vous ‡ temps partiel ?' : 'temps_partiel_premier_emploi',
'Comment avez-vous trouvÈ ce premier emploi ?' : 'moyen_trouver_premier_emploi',
'Quel fut votre principal critËre de choix sur ce 1er emploi?' : 'critere_choix_premier_emploi',
'Quel Ètait votre rÈmunÈration brute annuelle (HORS PRIMES et 13Ëme mois) ?' : 'remuneration_annuelle_brute_hors_prime_premier_emploi',
'Perceviez vous des primes?' : 'prime_premier_emploi',
'Quel Ètait votre rÈmunÈration brut AVEC PRIMES et 13Ëme mois?' : 'remuneration_annuelle_brute_avec_prime_premier_emploi',
'Globalement, vous Ítes satisfait(e) de votre premier emploi?' : 'satisfaction_premier_emploi',
'Globalement,sur une Èchelle allant de 1 ‡ 5, Ítes-vous satisfait(e) de votre formation ‡ Polytech Montpellier ? ' : 'satisfaction_formation',
'Votre double compÈtence en Informatique & Gestion vous semble-t-elle  constituer un plus pour votre insertion professionnelle ? ' : 'ig_avantage_double_competence',
'Etes-vous membre de rÈseaux ou associations  professionnelles liÈes ‡ l informatique  ? ' : 'ig_membre_reseau',
'Quels sont vos projets d Èvolution de carriËre ?' : 'projet_evolution_carriere',
'Recommanderiez-vous Polytech Montpellier ‡ un(e) ami(e) qui souhaite poursuivre un cursus dans l enseignement supÈrieur ?' : 'recommandation_polytech',
'Quel est le poste que vous occupez?' : "intitule_emploi",


},inplace=True)

data.drop(columns= ['Sans activitÈ volontairement, merci de prÈciser :','Vous occupez plusieurs emplois?','Pour quelle(s) raison(s) avez-vous refusÈ ce ou ces emploi(s) ?', 'Quel type d Ètudes poursuivez-vous ?', 'Concours prÈparÈ(s)', 'Dans quel Ètablissement Ítes-vous inscrit(e) (nom et ville Èventuellement) ?', 'vous poursuivez des Ètudes en alternance?','Pour quelle raison avez-vous principalement choisi de poursuivre des Ètudes ?', 'Quel type de thËse prÈparez-vous ?', 'Nom et coordonnÈes de l entreprise partenaire', 'Sujet de votre ThËse', 'Nom et coordonnÈes du laboratoire qui vous accueille', 'Le laboratoire est-il localisÈ?', 'RÈgion de localisation du laboratoire', 'Pays de localisation du laboratoire?', 
'Vous bÈnÈficiez d une source de financement contractuel, d une allocation ou d un contrat doctoral',
'Quel est le type de financement contractuel qui permet de financer votre thËse ?',
'De quel type de volontariat s agit-il ?',
'DurÈe du contrat de volontariat (en mois)',
'Pays ou territoire du volontariat',
'Montant brut annuel HORS PRIME de l allocation que vous percevez<br />',
'Montant brut annuelle AVEC primes, de l allocation que vous percevez?',
'Nom de l entreprise qui vous emploie :',
'L entreprise que vous crÈez/reprise est-elle dÈj‡ en activitÈ, ou ‡ l Ètat de projet/reprise plus ou moins avancÈ ?',
'annÈe de crÈation/reprise de l entreprise',
'Accompagnement par un incubateur et/ou une pÈpiniËre d entreprise?',
'Lequel ou lesquels?',
'Quel est le nom de votre entreprise / projet?',
'Une rapide description de son activitÈ',
'Quel est son secteur d activitÈ ?',
'Votre entreprise est/sera localisÈe?',
'RÈgion de localisation de la future entreprise',
'Pays de localisation de la future entreprise?',
'Le site web s il existe?',
'Quelle part de temps partiel rÈalisez-vous ?',
'Disposez-vous d une RQTH (Reconnaissance de la qualitÈ de Travailleur HandicapÈ)?',
'Adresse ( numero, rue, avenue..)de l entreprise',
'Code postal de l entreprise',
'Commune de l entreprise',
' Son site internet?',
'Plus prÈcisement dans la Production et distribution d eau assainissement, gestion des dÈchets et dÈpollution, il s agit de:',
'Plus prÈcisement dans le domaine du transport, il s agit :',
'Plus prÈcisement dans le domaine de l Èlaboration et caractÈrisation des matÈriaux, il s agit :',
'Plus prÈcisement dans le domaine de l  Energie, Èlectronique, Èlectrotechnique, il s agit :',
'Plus prÈcisement dans le domaine du BTP, il s agit :',
'Quels ont ÈtÈ les moyens principaux utilisÈs pour trouver votre emploi actuel?',
'Positionnez l activitÈ de votre entreprise dans les domaines suivants :',
'Positionnez votre propre activitÈ dans les domaines suivants :',
'Pourquoi considÈrez vous votre emploi comme directement liÈ aux energies renouvelables?',
'Votre rÈmunÈration brute annuelle (hors primes et 13Ëme mois)?',
'Percevez des primes et/ou un 13Ëme mois?',
'Votre rÈmunÈration brute annuelle AVEC primes et 13Ëme mois',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - Allemand',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - Espagnol',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - Chinois',
'Utilisez-vous frÈquemment les langues suivantes dans votre travail ? - Autre langue',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - La rÈputation de la filiËre de formation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - La formation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - La rÈputation de l Ècole/rÈseau Polytech',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - RÈseau des anciens (Polytech connect)',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Stage de 4Ëme annÈe',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Stage de 5Ëme annÈe /Contrat de professionnalisation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Vos compÈtences linguistiques',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Vos expÈriences ‡ l Ètranger ',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Vos expÈriences ‡ l Ècole (BDE, GEPI, startup we..)',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Votre expÈrience professionnelle (essentiellement s il ne s agit pas du 1er poste)',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement? - Vos engagements dans des associations et/ou projets ',
'Quels sont les aspects du Developpement Durable/ResponsabilitÈ sociÈtale qui vous concernent dans votre mission?',
'Votre entreprise pourrait-elle Ítre interessÈe par un ou des Ètudiants en: - apprentissage domaine Eau et GÈnie Civil',
'Votre entreprise pourrait-elle Ítre interessÈe par un ou des Ètudiants en: - apprentissage MÈcanique structures industrielles chaudronnÈes',
'Votre entreprise pourrait-elle Ítre interessÈe par un ou des Ètudiants en: - apprentissage SystËmes embarquÈs',
'Votre entreprise pourrait-elle Ítre interessÈe par un ou des Ètudiants en: - contrat de professionnalisation',
'Avez-vous refusÈ une ou plusieurs proposition(s) d emploi depuis ?',
'Depuis combien de temps Ètiez-vous en recherche d emploi ?',
'Pour quelle(s) raison(s) aviez-vous refusÈ ce ou ces emploi(s) ?',
'Quelle(s) difficultÈ(s) rencontriez-vous dans votre recherche d emploi?',
'Quel type d Ètudes poursuiviez-vous ?',
'Concours prÈparÈ(s)',
'Dans quel Ètablissement Ètiez-vous inscrit(e) (nom et ville Èventuellement) ?',
'Pour quelle raison aviez-vous principalement choisi de poursuivre des Ètudes ?',

'La poursuite d Ètude a-t elle jouÈ un rÙle preponderant dans votre insertion professionnelle?',
'De quel type de volontariat s agissait-il ?',
'DurÈe du contrat de volontariat (en mois)',
'Pays ou territoire du volontariat',
'Montant brut annuelle HORS primes, de l allocation perÁue?',
'Montant brut annuelle AVEC primes, de l allocation perÁue?',
'Nom de l entreprise qui vous a employÈ :',
'Quel type de thËse prÈpariez-vous ?',
'Nom et coordonnÈes de l entreprise partenaire',
'Sujet de ThËse',
'Nom et coordonnÈes du laboratoire qui vous a accueilli',
'Le laboratoire est-il localisÈ?',
'RÈgion de localisation du laboratoire',
'Pays de localisation du laboratoire?',
'Votre rÈmunÈration brute annuelle (hors primes et hors 13Ëme mois)?',
'Perceviez-vous des primes et/ou un 13Ëme mois?',
'Votre rÈmunÈration brute annuelle AVEC primes et/ou 13Ëme mois?',
'La thËse a-t elle jouÈ un rÙle preponderant dans votre insertion professionnelle?',
'A quel stade de la crÈation/reprise d entreprise  vous trouviez-vous ?',
'AnnÈe de crÈation /reprise?',
'Quel est (ou Ètait) le nom de l entreprise?',
'Et son activitÈ?',
'Etiez-vous accompagnÈ par un incubateur et/ou une pÈpiniËre d entreprise?',
'Lequel ou lesquels?',
'Quel Ètait le nom de votre entreprise / projet?',
'Une rapide description de votre activitÈ',
'Quel etait son secteur d activitÈ ?',
'Le site web s il existe?',
'Aviez-vous un statut de :',
'Adresse ( numero, rue, avenue..)de l entreprise',
'Code postal de l entreprise',
'Commune de l entreprise',
'Ce temps partiel Ètait-il voulu ou subi ?',
'Quelle part de temps partiel rÈalisiez-vous ?',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - La rÈputation du dÈpartement',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - La formation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - La rÈputation de l Ècole/rÈseau Polytech',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - RÈseau des anciens (Polytech Connect)',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - Stage de 5Ëme annÈe/Contrat de professionnalisation',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - Vos compÈtences linguistiques',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - Vos experiences ‡ l Ètranger ',
'Les ÈlÈments suivants vous semblent-ils avoir jouÈ un rÙle dans votre recrutement sur ce 1er emploi? - Vos experiences ‡ l Ècole (BDE, GEPI, startup we..)',
'L accompagnement de votre Ècole dans votre projet professionnel vous a-t-il paru?',
'Quels enseignements vous semblent les plus utiles pour l exercice de votre mÈtier et votre insertion professionnelle ?',
'Parmi les enseignements fournis par l Ècole, quels sont ceux qui mÈriteraient d Ítre approfondis ou renforcÈs ?',
'Quels enseignements, absents de votre formation, vous auraient ÈtÈ utiles ?',
'Quels enseignements, prÈsents dans votre formation, vous paraissent inutiles ?',
'Laquelle?',
'Souhaitez vous Ítre informÈ sur l  association des diplÙmÈs de Polytech (Polytech Connect)?',
'Accepteriez-vous d Ítre sollicitÈ par l Ècole pour intervenir dans la formation ?',
'Sous quelle(s) forme(s), dÈsireriez-vous intervenir?',
'Accepteriez-vous de servir de relais au sein de votre entreprise afin de faciliter la collecte de la taxe d apprentissage par l Ècole ?',
'Depuis que vous avez quittÈ l Ècole, Ítes-vous encore en contact avec des enseignants du dÈpartement?',
'Quels conseils pourriez-vous donner aux Ètudiants actuellement en formation pour bien choisir leur stage de fin d Ètude ? rÈussir leur insertion professionnelle ?',
'Vos remarques et commentaires relatifs ‡ votre insertion professionnelle',

], inplace=True)


data.to_csv('enquete_2020DS_nettoyer.csv', index=False)

#python3 netoyer.py