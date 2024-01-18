#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)


data <- read.csv("merged_database.csv", header = TRUE)
data_axe1 <- read.csv('donnees_combinees_filtrees.csv', header = TRUE)
merged_data <-data


data$filiere_combined2[data$filiere %in% c("Eau et Génie Civil (EGC - apprentissage)", "Eau et GÈnie Civil (EGC − apprentissage)")] <- "EGC"
data$filiere_combined2[data$filiere %in% c("Génie Biologique et Agroalimentaires (GBA)", "GÈnie Biologique et Agroalimentaires (GBA)")] <- "GBA"
data$filiere_combined2[data$filiere %in% c("Matériaux (MAT)", "MatÈriaux (MAT)")] <- "MAT"
data$filiere_combined2[data$filiere %in% c("Mécanique et Interactions (MI)", "MÈcanique et Interactions (MI)")] <- "MI"
data$filiere_combined2[data$filiere %in% c("Mécanique Structures Industrielles (MSI − apprentissage)", "MÈcanique Structures Industrielles (MSI − apprentissage)")] <- "MSI"
data$filiere_combined2[data$filiere %in% c("Microélectronique Et Automatique (MEA)", "MicroÈlectronique Et Automatique (MEA)")] <- "MEA"
data$filiere_combined2[data$filiere %in% c("Sciences et Technologies de l Eau (STE)", "Sciences et Technologies de l'Eau (STE)")] <- "STE"
data$filiere_combined2[data$filiere %in% c ("Energétique - énergies Renouvelables (EnR)")] <- "EnR"
data$filiere_combined2[data$filiere %in% c ("Informatique et Gestion (IG)")] <- "IG"
data$filiere_combined2[data$filiere %in% c("Systèmes Embarqués (SE - apprentissage)", "Systèmes EmbarquÈs (SE - apprentissage)")] <- "SE"
data$filiere_combined2[data$filiere %in% c("Mécanique Structures Industrielles (MSI - apprentissage)", "MÈcanique Structures Industrielles (MSI - apprentissage)")] <- "MSI"

data$type_combined <- ifelse(data$type_formation %in% c("Sous contrat d apprentissage", "Sous contrat d'apprentissage"),
                             "Sous contrat d'apprentissage", data$type_formation)

filtered_data <- data[!is.na(data$remuneration_prime) & data$type_combined != "", ]


filtered_data2 <- data[!is.na(data$remuneration_prime) & data$filiere_combined2 != ""& data$filiere_combined2 != "NA",]


moyennes_par_groupe <- data %>%
  group_by(filiere_combined2, type_combined, date_diplome) %>%
  summarise(
    moyenne_remuneration = mean(remuneration_prime, na.rm = TRUE),
  )

data1 <- read.csv("merged_database.csv") %>%
  filter(!is.na(secteur_premiere_entreprise) & secteur_premiere_entreprise != "",
         !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
         !is.na(sexe) & sexe != "")

data1$secteur_premiere_entreprise <- gsub("'", " ", data1$secteur_premiere_entreprise)
data2 <- read.csv("merged_database.csv") %>%
  filter(         !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "", !is.na(localisation_premier_emploi) & localisation_premier_emploi != "")

# Standardiser les valeurs de localisation
data2$localisation_premier_emploi <- tolower(iconv(data2$localisation_premier_emploi, to = "UTF-8", sub = "byte"))
data2$localisation_premier_emploi <- stringi::stri_trans_general(data2$localisation_premier_emploi, "Latin-ASCII")
data2$localisation_premier_emploi <- gsub("'", " ", data2$localisation_premier_emploi)

# Remplacer les valeurs spécifiques dans la colonne 'pays_premier_emploi'
data2 <- data2 %>%
  mutate(pays_premier_emploi = ifelse(pays_premier_emploi == "Je travaillais depuis la Belgique pour une entreprise Bangladaise/Allemande", "Belgique", pays_premier_emploi)) %>%
  mutate(pays_premier_emploi = ifelse(pays_premier_emploi %in% c("Danemark", "Danemark "), "Danemark", pays_premier_emploi))

# Ajouter "France" à la variable pays_premier_emploi lorsque localisation_premier_emploi est vide
data2 <- data2 %>%
  mutate(pays_premier_emploi = ifelse(localisation_premier_emploi == "en france", "France", pays_premier_emploi))
data3 <- read.csv("merged_database.csv") %>%
  filter(!is.na(filiere) & filiere != "",
         !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
         !is.na(date_diplome) & date_diplome != "")

data3$filiere <- fct_collapse(data3$filiere,
                              "Eau et Génie Civil (EGC - apprentissage)" = c("Eau et Génie Civil (EGC - apprentissage)", "Eau et GÈnie Civil (EGC - apprentissage)"),
                              "Génie Biologique et Agroalimentaires" = c("Génie Biologique et Agroalimentaires (GBA)", "GÈnie Biologique et Agroalimentaires (GBA)"),
                              "Matériaux" = c("Matériaux (MAT)", "MatÈriaux (MAT)"),
                              "Mécanique et Interactions" = c("Mécanique et Interactions (MI)", "MÈcanique et Interactions (MI)"),
                              "Mécanique Structures Industrielles" = c("Mécanique Structures Industrielles (MSI - apprentissage)", "MÈcanique Structures Industrielles (MSI - apprentissage)"),
                              "Microélectronique Et Automatique" = c("Microélectronique Et Automatique (MEA)", "MicroÈlectronique Et Automatique (MEA)"),
                              "Sciences et Technologies de l'Eau" = c("Sciences et Technologies de l'Eau (STE)", "Sciences et Technologies de l Eau (STE)"),
                              "Systèmes Embarqués" = c("Systèmes Embarqués (SE - apprentissage)", "SystËmes EmbarquÈs (SE - apprentissage)")
)


merged_data$region_combined <- merged_data$region_emploi

# Définir les variations à combiner
variations_a_combiner_reunion <- c("Réunion", "RÈunion", "La Réunion")
variations_a_combiner_provence <- c("Provence-Alpes-CÙte d'Azur", "Provence-Alpes-Côte d Azur")
variations_a_combiner_collectivite <- c("CollectivitÈs d'Outre-Mer (COM)")
variations_a_combiner_rhone <- c("Auvergne-RhÙne-Alpes")
variations_a_combiner_bourg <- c("Bourgogne-Franche-ComtÈ")
variations_a_combiner_ile <- c("Œle-de-France")



# Mettre à jour la nouvelle variable pour regrouper les variations
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_reunion] <- "La Réunion"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_provence] <- "Provence-Alpes-Côte d'Azur"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_collectivite] <- "Collectivités d'Outre-Mer (COM)"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_rhone] <- "Auvergne-Rhône-Alpes"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_bourg] <- "Bourgogne-Franche-Comté"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_ile] <- "Île-de-France"


merged_data$taille_combined[merged_data$taille_entreprise %in% c(" Moins de 10 salariÈ(e)s ", " Moins de 10 salarié(e)s ")] <- "Moins de 10 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("250 ‡  4 999 salariÈ(e)s ", "250 à  4 999 salarié(e)s ")] <- "250 à 4 999 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("5 000 salarié(e)s ou plus", "5 000 salarié(e)s ou plus")] <- "5 000 salarié(e)s ou plus"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 10 ‡ 19 salariÈ(e)s ", "De 10 à 19 salarié(e)s ")] <- "De 10 à 19 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 20 ‡ 49 salariÈ(e)s ", "De 20 à 49 salarié(e)s ")] <- "De 20 à 49 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 50 ‡ 249 salariÈ(e)s ", "De 50 à 249 salarié(e)s ")] <- "De 50 à 249 salarié(e)s"


merged_data$region_combined <- na.omit(merged_data$region_combined)


ui <- fluidPage(

  titlePanel("Analyses Formation App"),

  tabsetPanel(

    tabPanel("Histogrammes",
             h3("Histogrammes"),

             h4("Répartition des salaires par filière"),
             plotOutput("histogram_filiere"),

             h4("Répartition des salaires par type de formation"),
             plotOutput("histogram_typeformation"),

             h4("Répartition des salaires par date d'obtention du diplôme"),
             plotOutput("histogram_datediplome")

    ),

    tabPanel("Filieres",
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("filieres", "Sélectionner des filières", choices = c("MEA","SE","EnR","MSI", "IG", "MAT", "MI", "GBA","STE", "EGC"), selected = c("MEA","SE","EnR","MSI", "IG", "MAT", "MI", "GBA","STE", "EGC")),
                 checkboxGroupInput("sexe", "Sélectionner le sexe", choices = c("Homme","Femme"), selected = c("Homme", "Femme"))
               ),
               mainPanel(
                 plotOutput("boxplot_filiere"),
                 plotOutput("boxplot_unique_filiere")

               )
             )
    ),
    tabPanel("Type de Formation",
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("typesformation", "Sélectionner des types de formation", choices = c("Formation initiale (hors apprentissage)","Sous contrat d'apprentissage","Sous contrat de professionalisation"), selected = c("Formation initiale (hors apprentissage)","Sous contrat d'apprentissage","Sous contrat de professionalisation")),
                 checkboxGroupInput("sexes", "Sélectionner le sexe", choices = c("Homme","Femme"), selected = c("Homme", "Femme"))
               ),
               mainPanel(
                 plotOutput("boxplot_typeformation"),
                 plotOutput("boxplot_unique_typeformation")

               )
             )
    ),
    tabPanel("Obtention diplome",
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("datediplome", "Sélectionner des types de formation", choices = c("2015","2016","2017","2018","2019","2020","2021","2022"), selected = c("2015","2016","2017","2018","2019","2020","2021","2022")),
                 checkboxGroupInput("sexess", "Sélectionner le sexe", choices = c("Homme","Femme"), selected = c("Homme", "Femme"))
               ),
               mainPanel(
                 plotOutput("boxplot_datediplome")
               )

               ),
             tabsetPanel(
               tabPanel("Boxplot par Filière", plotOutput("boxplot_unique_datediplome")),
             )
    ),
    tabPanel("Analyse des Moyennes par Groupe",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("filieress", "Sélectionnez une filière", choices = c("MEA","SE","EnR","MSI", "IG", "MAT", "MI", "GBA","STE", "EGC")),
                 checkboxGroupInput("type_formationss", "Sélectionnez un type de formation", choices = c("Formation initiale (hors apprentissage)","Sous contrat d'apprentissage","Sous contrat de professionalisation")),
                 checkboxGroupInput("date_diplomess", "Sélectionnez une date de diplomation", choices = c("2015","2016","2017","2018","2019","2020","2021","2022"))
               ),
               mainPanel(
                 plotOutput("moyennes_plot")
               )
             ),
    ),

    tabPanel("Analyse Secteur d'activité Salaire Premier Emploi",
             sidebarLayout(
               sidebarPanel(
             checkboxGroupInput("genderInput", "Sélectionnez le sexe:",
                                choices = c("Homme", "Femme"), selected = "Homme"),
             checkboxGroupInput("sectorInput", "Sélectionnez le secteur d'activité:",
                                choices = unique(data1$secteur_premiere_entreprise), selected = unique(data1$secteur_premiere_entreprise)),
              ),
             mainPanel(
             plotOutput("plot1"))
             ),
    ),
    tabPanel("Analyse Localisation géographique Salaire Premier Emploi",
             sidebarLayout(
               sidebarPanel(
             checkboxGroupInput("localisationInput", "Sélectionnez la localisation:",
                                choices = unique(data2$localisation_premier_emploi), selected = unique(data2$localisation_premier_emploi)),
             checkboxGroupInput("countryInput", "Sélectionnez le pays:",
                                choices = unique(data2$pays_premier_emploi), selected = unique(data2$pays_premier_emploi)),
               ),
             mainPanel(
             plotOutput("plot2"))
             ),
    ),
    tabPanel("Filière et Date de Diplôme Salaire Premier Emploi",
             sidebarLayout(
               sidebarPanel(
             checkboxGroupInput("filiereInput", "Sélectionnez la filière:",
                                choices = unique(data3$filiere), selected = unique(data3$filiere)),
             checkboxGroupInput("dateDiplomeInput", "Sélectionnez la date de diplôme:",
                                choices = unique(data3$date_diplome), selected = unique(data3$date_diplome)),
               ),
             mainPanel(
             plotOutput("plot3"))
             )
    ),
    tabPanel("Genre",
             h3("Genre"),
                h4("Distribution des réponses"),
                plotOutput("camembert_reponses"),

                h4("Distribution des réponses par genre par filière"),
                plotOutput("diagramme_empile_GF"),

                h4("Rémunération annuelle brute par genre"),
                plotOutput("boxplots_Genre")

    ),
    tabPanel("Rémunération Annuelle Brute par Filière",
             h4("Diagrammes de boîtes par Filière"),
             plotOutput("boxplot_remuneration_filière")
    ),
    tabPanel("Nationalité",
             h3("Nationalité"),
                h4("Distribution des réponses des français par rapport aux étrangers"),
                plotOutput("camembert_nationalite"),

    ),

      tabPanel("Région entreprise",
               sidebarLayout(
                 sidebarPanel(
                   # Cases à cocher pour choisir la région
                   checkboxGroupInput("regions", "Sélectionner des régions", choices = unique(merged_data$region_combined)),
                   # Cases à cocher pour choisir le sexe
                   checkboxGroupInput("sexe_region", "Sélectionner le sexe", choices = c("Homme", "Femme"))
                 ),
                 mainPanel(
                   # Affichage du boxplot
                   plotOutput("boxplot1"),
                   plotOutput("barplot_region")
                 )
               )
    ),
      # Deuxième onglet
      tabPanel("Taille entreprise",
               sidebarLayout(
                 sidebarPanel(
                   # Cases à cocher pour choisir la région
                   checkboxGroupInput("regions_t", "Sélectionner des régions", choices = unique(merged_data$region_combined)),
                   # Cases à cocher pour choisir le sexe
                   # Cases à cocher pour choisir la taille de l'entreprise
                   checkboxGroupInput("taille", "Sélectionner la taille de l'entreprise", choices = unique(attribut_entreprise$taille_combined))
                 ),
                 mainPanel(
                   # Affichage du boxplot
                   plotOutput("boxplot_taille")
                 )
               )
      ),
      tabPanel("Satisfaction Emploi",
               sidebarLayout(
                 sidebarPanel(
                   # Filtres pour l'analyse de la satisfaction des employés
                   checkboxGroupInput("satisfaction_sexe", "Sélectionner le sexe", choices = c("Homme", "Femme")),
                   sliderInput("satisfaction_remuneration", "Filtrer par rémunération", min = 25000, max = 70000, value = c(25000, 70000)),
                   # ... Ajoutez d'autres filtres selon vos besoins
                 ),
                 mainPanel(
                   plotOutput("satisfaction_plot1"),
                   plotOutput("satisfaction_plot2")
                 )
               )
      )


    )
)

server <- function(input, output) {

  output$histogram_filiere <- renderPlot({


    histogram3 <- ggplot(data, aes(x = filiere_combined2, y = remuneration_prime, fill = filiere_combined2)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Répartition des salaires par filière",
           x = "Filière",
           y = "Rémunération Prime") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal()
    print(histogram3)
  })

  output$histogram_typeformation <- renderPlot({


    histogram <- ggplot(filtered_data, aes(x = type_combined, y = remuneration_prime, fill = type_combined)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Répartition des salaires par type de formation",
           x = "Type de Formation",
           y = "Rémunération Prime") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
    print(histogram)
  })

  data$date_diplome <- as.factor(data$date_diplome)

  output$histogram_datediplome <- renderPlot({
    histogram2 <- ggplot(subset(data, date_diplome != "721"), aes(x = date_diplome, y = remuneration_prime, fill = date_diplome)) +
      geom_col(alpha = 0.7) +
      labs(title = "Répartition des salaires par date d'obtention du diplôme",
           x = "Date d'obtention du diplôme",
           y = "Rémunération Prime") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()

    print(histogram2)
  })

  output$boxplot_unique_filiere <- renderPlot({
    ggplot(filtered_data2, aes(x = factor(filiere_combined2), y = remuneration_prime)) +
      geom_boxplot(width = 0.6) +
      labs(title = "Boxplot par filière",
           x = "Filière",
           y = "Rémunération Prime") +
      theme_minimal()
  })

  # Boxplot pour type_combined
  output$boxplot_unique_typeformation <- renderPlot({
    ggplot(filtered_data, aes(x = factor(type_combined), y = remuneration_prime)) +
      geom_boxplot(width = 0.6) +
      labs(title = "Boxplot par type de formation",
           x = "Type de Formation",
           y = "Rémunération Prime") +
      theme_minimal()
  })

  # Boxplot pour date_diplome
  output$boxplot_unique_datediplome <- renderPlot({
    ggplot(subset(data, date_diplome != "721"), aes(x = date_diplome, y = remuneration_prime)) +
      geom_boxplot(width = 0.7) +
      labs(title = "Boxplot par date d'obtention du diplôme",
           x = "Date d'obtention du diplôme",
           y = "Rémunération Prime") +
      theme_minimal()
  })


  output$diagramme_empile_GF <- renderPlot({
    ggplot(data_axe1, aes(x = filiere, fill = sexe)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Homme" = "#4477AA", "Femme" = "#EE6677", "Ne souhaite pas répondre" = "grey50")) +
      labs(x = "Filière", y = "Pourcentage", fill = "Sexe",
           title = "Répartition par Sexe au sein de chaque Filière") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  })

  output$camembert_reponses <- renderPlot({
    # Compter le nombre de réponses par sexe
    reponses_par_sexe <- data_axe1 %>%
      group_by(sexe) %>%
      summarise(Nombre = n()) %>%
      ungroup()

    ggplot(reponses_par_sexe, aes(x = "", y = Nombre, fill = sexe)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("Femme" = "#EE6677",
                                   "Homme" = "#4477AA",
                                   "Ne souhaite pas répondre" = "grey50")) +
      labs(title = "Répartition des Réponses par Genre",
           fill = "Sexe") +
      theme(legend.title = element_blank())

  })


  output$camembert_nationalite <- renderPlot({
    reponses_nationalite <- data_axe1 %>%
      mutate(nationalite_francaise = ifelse(nationalite_francaise == "", "Pas de réponse", nationalite_francaise)) %>%
      count(nationalite_francaise) %>%
      ungroup()

    # Créer un diagramme en camembert
    ggplot(reponses_nationalite, aes(x = "", y = n, fill = nationalite_francaise)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      labs(fill = "Nationalité Française",
           title = "Proportion des Répondants par Statut de Nationalité Française") +
      scale_fill_brewer(palette = "Set1") +
      theme_void() +
      theme(legend.title = element_blank(), legend.position = "bottom")

  })

  output$boxplots_Genre <- renderPlot({
    # le boxplot des salaires par genre
        library(dplyr)

    # Calculer les outliers manuellement
    outliers <- data_axe1 %>%
      group_by(sexe) %>%
      summarize(
        lower = quantile(remuneration_annuelle_brute, probs = 0.25) - 1.5 * IQR(remuneration_annuelle_brute),
        upper = quantile(remuneration_annuelle_brute, probs = 0.75) + 1.5 * IQR(remuneration_annuelle_brute),
        .groups = 'drop'  # Ajouté pour éviter les avertissements dans dplyr 1.0.0 et plus
      ) %>%
      left_join(data_axe1, by = "sexe") %>%
      filter(remuneration_annuelle_brute < lower | remuneration_annuelle_brute > upper)

    # Créer le boxplot de base sans outliers
    base_plot <- ggplot(data_axe1, aes(x = sexe, y = remuneration_annuelle_brute)) +
      geom_boxplot(outlier.shape = NA) +  # Suppression des outliers dans cette couche
      labs(title = "Salaires par Genre", x = "Sexe", y = "Salaire Annuel Brut")

    # Ajouter les outliers avec des couleurs spécifiques pour chaque sexe
    final_plot <- base_plot +
      geom_point(data = outliers, aes(x = sexe, y = remuneration_annuelle_brute, colour = sexe), shape = 1) +
      scale_colour_manual(values = c("Homme" = "blue", "Femme" = "red", "Ne souhaite pas répondre" = "grey")) +
      theme(legend.position = "none")  # Cache la légende

    # Afficher le graphique final
    print(final_plot)
  })



  output$boxplot_filiere <- renderPlot({
    filtered_data3 <- data[!is.na(data$remuneration_prime) &
                             data$filiere_combined2 %in% input$filieres &
                             data$sexe %in% input$sexe, ]

    ggplot(filtered_data3, aes(x = factor(filiere_combined2), y = remuneration_prime, fill = factor(sexe))) +
      geom_boxplot(width = 0.6) +
      labs(x = "Filière", y = "Rémunération Prime") +
      ggtitle(paste("Variation de la rémunération Prime pour les filières sélectionnées")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }, height = 600, width = 1200)

  output$boxplot_typeformation <- renderPlot({
    filtered_data4 <- data[!is.na(data$remuneration_prime) &
                             data$type_combined %in% input$typesformation &
                             data$sexe %in% input$sexes, ]

    ggplot(filtered_data4, aes(x = factor(type_combined), y = remuneration_prime, fill = factor(sexe))) +
      geom_boxplot(width = 0.6) +
      labs(x = "Type de Formation", y = "Rémunération Prime") +
      ggtitle(paste("Variation de la rémunération Prime par type de formation")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }, height = 600, width = 1200)

  output$boxplot_datediplome <- renderPlot({
    filtered_data5 <- data[!is.na(data$remuneration_prime) &
                             data$date_diplome %in% input$datediplome &
                             data$sexe %in% input$sexess &
                             data$date_diplome != "721" &
                             data$date_diplome != "NA", ]

    ggplot(filtered_data5, aes(x = factor(date_diplome), y = remuneration_prime, fill = factor(sexe))) +
      geom_boxplot(width = 0.6) +
      labs(x = "Date d'obtention du diplôme", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime par date d'obtention du diplôme") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }, height = 600, width = 1200)

  filtered_data_moyenne <- reactive({
    filter_data_moyenne <- moyennes_par_groupe
    if (!is.null(input$filieress)) {
      filter_data_moyenne <- filter_data_moyenne[filter_data_moyenne$filiere_combined2 == input$filieress, ]
    }
    if (!is.null(input$type_formationss)) {
      filter_data_moyenne <- filter_data_moyenne[filter_data_moyenne$type_combined == input$type_formationss, ]
    }
    if (!is.null(input$date_diplomess)) {
      filter_data_moyenne <- filter_data_moyenne[filter_data_moyenne$date_diplome == input$date_diplomess, ]
    }
    filter_data_moyenne
  })

  output$moyennes_plot <- renderPlot({
    ggplot(filtered_data_moyenne(), aes(x = factor(filiere_combined2), y = moyenne_remuneration, fill = factor(type_combined), fill = factor(date_diplome))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Moyennes par Groupe",
           x = "Filière",
           y = "Moyenne de Rémunération") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })

  output$boxplot_remuneration_filière <- renderPlot({
    ggplot(donnees_combinees_filtrees, aes(x = filiere, y = remuneration_annuelle_brute, fill = filiere)) +
      geom_boxplot() +
      scale_fill_manual(values = c("GBA" = "chartreuse3", "IG" = "deeppink1", "MEA" = "plum", "STE" = "deepskyblue3",
                                   "MI"="salmon3", "SE-app"="skyblue3", "MSI-app"="lightsteelblue", "MAT"="lightseagreen", "EnR"="wheat1", "EGC-app"="sienna1")) +  # Définir les couleurs manuellement
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Incliner les étiquettes de l'axe x si elles sont trop longues
      labs(title = "Boxplot de la Rémunération Annuelle Brute par Filière",
           x = "Filière",
           y = "Rémunération Annuelle Brute") +
      theme_minimal() +
      theme(legend.position = "none")

  })


  # Diagramme 1 - Rémunération en fonction du secteur d'activité
  output$plot1 <- renderPlot({
    data1 <- data %>%
      filter(!is.na(secteur_premiere_entreprise) & secteur_premiere_entreprise != "",
             !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
             !is.na(sexe) & sexe != "")

    if ("Homme" %in% input$genderInput && "Femme" %in% input$genderInput) {
      # No filter on gender
    } else if ("Homme" %in% input$genderInput) {
      data1 <- filter(data1, sexe == "Homme")
    } else if ("Femme" %in% input$genderInput) {
      data1 <- filter(data1, sexe == "Femme")
    } else {
      # If no gender is selected, show all data
    }

    if (!is.null(input$sectorInput) && length(input$sectorInput) > 0) {
      data1 <- filter(data1, secteur_premiere_entreprise %in% input$sectorInput)
    }

    data1$secteur_premiere_entreprise <- gsub("'", " ", data1$secteur_premiere_entreprise)

    ggplot(data1, aes(x = secteur_premiere_entreprise, y = remuneration_annuelle_brute_avec_prime_premier_emploi, fill = sexe)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0), width = 0.8) +
      labs(title = "Rémunération Premier Emploi en fonction du secteur d'activité",
           x = "Secteur",
           y = "Rémunération moyenne") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Diagramme 2 - Rémunération en fonction de la localisation géographique
  output$plot2 <- renderPlot({
    data2_filtered <- data2

    if (!is.null(input$localisationInput) && length(input$localisationInput) > 0) {
      data2_filtered <- filter(data2_filtered, localisation_premier_emploi %in% input$localisationInput)
    }

    if (!is.null(input$countryInput) && length(input$countryInput) > 0) {
      data2_filtered <- filter(data2_filtered, pays_premier_emploi %in% input$countryInput)
    }

    ggplot(data2_filtered, aes(x = localisation_premier_emploi, y = remuneration_annuelle_brute_avec_prime_premier_emploi, fill = pays_premier_emploi)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Rémunération Premier Emploi en fonction de la localisation géographique",
           x = "Localisation",
           y = "Rémunération moyenne") +
      theme_minimal()
  })





  observeEvent(input$selectAll1, {
    updateCheckboxGroupInput(session, "genderInput", selected = c("Homme", "Femme"))
    updateCheckboxGroupInput(session, "sectorInput", selected = unique(data1$secteur_premiere_entreprise))
  })

  observeEvent(input$deselectAll1, {
    updateCheckboxGroupInput(session, "genderInput", selected = character(0))
    updateCheckboxGroupInput(session, "sectorInput", selected = character(0))
  })

  observeEvent(input$selectAll2, {
    updateCheckboxGroupInput(session, "localisationInput", selected = unique(data2$localisation_premier_emploi))
    updateCheckboxGroupInput(session, "countryInput", selected = unique(data2$pays_premier_emploi))
  })

  observeEvent(input$deselectAll2, {
    updateCheckboxGroupInput(session, "localisationInput", selected = character(0))
    updateCheckboxGroupInput(session, "countryInput", selected = character(0))
  })

  # Diagramme 3 - Rémunération Premier Emploi en fonction de la Filière et de la Date de Diplôme
  output$plot3 <- renderPlot({
    data3_filtered <- data3

    if (!is.null(input$filiereInput) && length(input$filiereInput) > 0) {
      data3_filtered <- filter(data3_filtered, filiere %in% input$filiereInput)
    }

    if (!is.null(input$dateDiplomeInput) && length(input$dateDiplomeInput) > 0) {
      data3_filtered <- filter(data3_filtered, date_diplome %in% input$dateDiplomeInput)
    }

    ggplot(data3_filtered, aes(x = filiere, y = remuneration_annuelle_brute_avec_prime_premier_emploi, color = date_diplome)) +
      geom_point() +
      labs(title = "Rémunération Premier Emploi en fonction de la Filière et de la Date de Diplôme",
           x = "Filière",
           y = "Rémunération Annuelle Brute",
           color = "Date de Diplôme") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  observeEvent(input$selectAll3, {
    updateCheckboxGroupInput(session, "filiereInput", selected = unique(data3$filiere))
    updateCheckboxGroupInput(session, "dateDiplomeInput", selected = unique(data3$date_diplome))
  })

  observeEvent(input$deselectAll3, {
    updateCheckboxGroupInput(session, "filiereInput", selected = character(0))
    updateCheckboxGroupInput(session, "dateDiplomeInput", selected = character(0))
  })

  output$boxplotFiliereGenre <- renderPlot({
    if (!is.null(input$filiere)) {
      filtered_data <- data_axe1 %>%
        filter(filiere %in% input$filiere)
      boxplotFiliereGenre(filtered_data)
    }
  })

  output$boxplot1 <- renderPlot({
    # Filtrer les données en fonction des régions et du sexe sélectionnés, de la plage de rémunération, et exclure les valeurs NA
    filtered_data <- merged_data[
      !is.na(merged_data$remuneration_prime) &  # Exclure les NA
        !is.na(merged_data$region_combined) &
        merged_data$region_combined %in% input$regions &
        (merged_data$sexe %in% input$sexe_region ) &
        merged_data$remuneration_prime >= 25000 &
        merged_data$remuneration_prime <= 70000,
    ]

    # Créer le boxplot
    ggplot(filtered_data, aes(x = factor(region_combined), y = remuneration_prime, fill = factor(sexe))) +
      geom_boxplot() +
      labs(x = "Région emploi", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime pour les régions sélectionnées par sexe") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })

  output$boxplot_taille <- renderPlot({

    # Filtrer les données en fonction des régions, du sexe, de la taille de l'entreprise, de la plage de rémunération, et exclure les valeurs NA
    filtered_data2 <- merged_data[
      !is.na(merged_data$remuneration_prime) &  # Exclure les NA
        merged_data$region_combined %in% input$regions_t &
        (merged_data$remuneration_prime >= 25000 & merged_data$remuneration_prime <= 70000) &
        (merged_data$taille_combined %in% input$taille),
    ]
    # Créer le boxplot avec des couleurs différentes pour chaque taille d'entreprise
    ggplot(filtered_data2, aes(x = factor(region_combined), y = remuneration_prime, fill = factor(taille_combined))) +
      geom_boxplot() +
      labs(x = "Région emploi", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime pour les régions sélectionnées par sexe et taille d'entreprise") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })

  # Nouvelle sortie pour le graphique en barres
  output$barplot_region <- renderPlot({
    # Filtrer les données pour exclure les NA
    filtered_data <- merged_data[!is.na(merged_data$remuneration_prime), ]

    # Filtrer les données en fonction des régions sélectionnées par l'utilisateur
    filtered_data_region <- filtered_data[filtered_data$region_combined %in% input$regions, ]

    # Créer le graphique en barres avec la moyenne et des couleurs différentes
    ggplot(filtered_data_region, aes(x = factor(region_combined), y = remuneration_prime, fill = factor(region_combined))) +
      stat_summary(fun=mean, geom="bar", position="dodge", width=0.7, color="black", size=0.7) +  # Ajouter la barre de moyenne
      labs(x = "Région emploi", y = "Rémunération Prime") +
      ggtitle("Moyenne de la rémunération Prime par région de l'emploi (sans NA)") +
      scale_fill_discrete() +  # Utiliser des couleurs différentes
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })

  output$satisfaction_plot1 <- renderPlot({
    # Filtrer les données en fonction des filtres de satisfaction sélectionnés
    filtered_satisfaction_data <- merged_data[
      (merged_data$sexe %in% input$satisfaction_sexe) &
        (merged_data$remuneration_prime >= input$satisfaction_remuneration[1] & merged_data$remuneration_prime <= input$satisfaction_remuneration[2]),
    ]

    # Réaliser votre première analyse de satisfaction (par exemple, un graphique ou une autre visualisation)
    # Utilisez les colonnes appropriées de filtered_satisfaction_data et ajustez en fonction de vos besoins
    # Exemple :
    ggplot(filtered_satisfaction_data, aes(x = factor(filtered_satisfaction_data$satisfaction_emploi), y = filtered_satisfaction_data$remuneration_prime)) +
      geom_boxplot() +
      labs(x = "Niveau de satisfaction", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime par niveau de satisfaction")
  })

  output$satisfaction_plot2 <- renderPlot({
    # Filtrer les données en fonction des filtres de satisfaction sélectionnés
    filtered_satisfaction_data <- merged_data[
      (merged_data$remuneration_prime >= input$satisfaction_remuneration[1] & merged_data$remuneration_prime <= input$satisfaction_remuneration[2]),
    ]

    # Calculer la moyenne de la rémunération pour chaque niveau de satisfaction
    mean_satisfaction_by_level <- aggregate(remuneration_prime ~ satisfaction_emploi, data = filtered_satisfaction_data, FUN = mean)

    # Afficher les données agrégées pour déboguer
    print(mean_satisfaction_by_level)

    ggplot(mean_satisfaction_by_level, aes(x = satisfaction_emploi, y = remuneration_prime)) +
      geom_line(color = "blue", size = 1) +
      labs(x = "Niveau de satisfaction", y = "Moyenne de la rémunération Prime") +
      ggtitle("Moyenne de la rémunération Prime par niveau de satisfaction") +
      theme_minimal() +
      theme(axis.text.x = element_text(vjust = 0.5, hjust = 1))
  })

  boxplotFiliereGenre <- function(data) {
    ggplot(data, aes(x = filiere, y = remuneration_annuelle_brute)) +
      geom_boxplot(aes(fill = sexe), position = position_dodge(0.8), outlier.colour = "black", outlier.shape = 1) +
      scale_fill_manual(values = c("Femme" = "#EE6677", "Homme" = "#4477AA", "Ne souhaite pas répondre" = "grey50")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(title = "Boxplot de la Rémunération Annuelle Brute par Filière et Sexe",
           x = "Filière",
           y = "Rémunération Annuelle Brute") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_y_continuous(breaks = seq(0, 100000, by = 10000), labels = scales::comma, limits = c(0, 100000))
  }






}





shinyApp(ui = ui, server = server)

