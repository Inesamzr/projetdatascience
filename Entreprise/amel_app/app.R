# ... (le reste du code reste inchangé)

# Ajouter une nouvelle variable pour combiner les variations de "Réunion"
merged_data$region_combined <- merged_data$region_emploi

# Définir les variations à combiner
variations_a_combiner_reunion <- c("Réunion", "RÈunion", "La Réunion")
variations_a_combiner_provence <- c("Provence-Alpes-CÙte d'Azur", "Provence-Alpes-Côte d Azur")
variations_a_combiner_collectivite <- c("CollectivitÈs d'Outre-Mer (COM)")
variations_a_combiner_rhone <- c("Auvergne-RhÙne-Alpes")
variations_a_combiner_bourg <- c("Bourgogne-Franche-ComtÈ")

# Mettre à jour la nouvelle variable pour regrouper les variations
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_reunion] <- "La Réunion"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_provence] <- "Provence-Alpes-Côte d'Azur"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_collectivite] <- "Collectivités d'Outre-Mer (COM)"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_rhone] <- "Auvergne-Rhône-Alpes"
merged_data$region_combined[merged_data$region_emploi %in% variations_a_combiner_bourg] <- "Bourgogne-Franche-Comté"


merged_data$taille_combined[merged_data$taille_entreprise %in% c(" Moins de 10 salariÈ(e)s ", " Moins de 10 salarié(e)s ")] <- "Moins de 10 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("250 ‡  4 999 salariÈ(e)s ", "250 à  4 999 salarié(e)s ")] <- "250 à 4 999 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("5 000 salarié(e)s ou plus", "5 000 salarié(e)s ou plus")] <- "5 000 salarié(e)s ou plus"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 10 ‡ 19 salariÈ(e)s ", "De 10 à 19 salarié(e)s ")] <- "De 10 à 19 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 20 ‡ 49 salariÈ(e)s ", "De 20 à 49 salarié(e)s ")] <- "De 20 à 49 salarié(e)s"
merged_data$taille_combined[merged_data$taille_entreprise %in% c("De 50 ‡ 249 salariÈ(e)s ", "De 50 à 249 salarié(e)s ")] <- "De 50 à 249 salarié(e)s"


merged_data$region_combined <- na.omit(merged_data$region_combined)


# Définir UI pour l'application qui dessine un boxplot
ui <- fluidPage(
  titlePanel("Variation de la rémunération Prime par la région de l'emploi"),
  tabsetPanel(
    # Premier onglet
    tabPanel("Boxplot",
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
    tabPanel("Autre Onglet",
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


# Définir la logique du serveur nécessaire pour dessiner un boxplot
server <- function(input, output) {

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

}

# Exécuter l'application
shinyApp(ui = ui, server = server)
