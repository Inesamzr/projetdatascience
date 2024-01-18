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
                 checkboxGroupInput("sexe", "Sélectionner le sexe", choices = c("Homme", "Femme"))
               ),
               mainPanel(
                 # Affichage du boxplot
                 plotOutput("boxplot")
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
    )
  )
)


# ... (le reste du code reste inchangé)

# Définir la logique du serveur nécessaire pour dessiner un boxplot
server <- function(input, output) {

  output$boxplot <- renderPlot({
    # Filtrer les données en fonction des régions et du sexe sélectionnés, de la plage de rémunération, et exclure les valeurs NA
    filtered_data <- merged_data[
      !is.na(merged_data$remuneration_prime) &  # Exclure les NA
        !is.na(merged_data$region_combined) &
        merged_data$region_combined %in% input$regions &
        (merged_data$sexe %in% input$sexe ) &
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
}

# Exécuter l'application
shinyApp(ui = ui, server = server)
