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

library(shiny)
library(ggplot2)
library(dplyr)

library(shiny)
library(ggplot2)
library(dplyr)

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Variation de la rémunération annuelle brute par genre et nationalité"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("btnSalaires", "Salaires par Genre"),
      actionButton("btnSexe", "Réponses par Sexe"),
      actionButton("btnFiliere", "Rémunération Annuelle Brute par Filière"),
      actionButton("btnNationalite", "Nationalité"),
      actionButton("btnFiliereGenre", "Rémunération Annuelle Brute par Genre par Filière"),
      # Cases à cocher pour choisir la filèreGenre
      checkboxGroupInput("filiere", "Choisir des filières :", choices = unique(donnees_combinees_filtrees$filiere))
    ),
    mainPanel(
      plotOutput("activePlot")
    )
  )
)

# Définir la logique du serveur
server <- function(input, output) {
  data <- read.csv('donnees_combinees_filtrees.csv')
  
  observeEvent(input$btnSalaires, {
    output$activePlot <- renderPlot({
      boxplotSalaires(data)
    })
  })
  
  observeEvent(input$btnSexe, {
    output$activePlot <- renderPlot({
      pieChartSexe(data)
    })
  })
  
  observeEvent(input$btnFiliere, {
    output$activePlot <- renderPlot({
      boxplotFiliere(data)
    })
  })
  
  observeEvent(input$btnNationalite, {
    output$activePlot <- renderPlot({
      pieChartNationalite(data)
    })
  })
  
  observeEvent(input$btnFiliereGenre, {
    output$activePlot <- renderPlot({
      if (!is.null(input$filiere)) {
        filtered_data <- data %>%
          filter(filiere %in% input$filiere)
        boxplotFiliereGenre(filtered_data)
      }
    })
  })
  
  
}

# Fonction pour le boxplot des salaires par genre
boxplotSalaires <- function(data) {
  # Insérer le code pour le boxplot des salaires par genre
  
  # Charger la bibliothèque dplyr pour le traitement des données
  library(dplyr)
  
  # Calculer les outliers manuellement
  outliers <- donnees_combinees_filtrees %>%
    group_by(sexe) %>%
    summarize(
      lower = quantile(remuneration_annuelle_brute, probs = 0.25) - 1.5 * IQR(remuneration_annuelle_brute),
      upper = quantile(remuneration_annuelle_brute, probs = 0.75) + 1.5 * IQR(remuneration_annuelle_brute),
      .groups = 'drop'  # Ajouté pour éviter les avertissements dans dplyr 1.0.0 et plus
    ) %>%
    left_join(donnees_combinees_filtrees, by = "sexe") %>%
    filter(remuneration_annuelle_brute < lower | remuneration_annuelle_brute > upper)
  
  # Créer le boxplot de base sans outliers
  base_plot <- ggplot(donnees_combinees_filtrees, aes(x = sexe, y = remuneration_annuelle_brute)) +
    geom_boxplot(outlier.shape = NA) +  # Suppression des outliers dans cette couche
    labs(title = "Salaires par Genre", x = "Sexe", y = "Salaire Annuel Brut")
  
  # Ajouter les outliers avec des couleurs spécifiques pour chaque sexe
  final_plot <- base_plot + 
    geom_point(data = outliers, aes(x = sexe, y = remuneration_annuelle_brute, colour = sexe), shape = 1) +
    scale_colour_manual(values = c("Homme" = "blue", "Femme" = "red", "Ne souhaite pas répondre" = "grey")) +
    theme(legend.position = "none")  # Cache la légende
  
  # Afficher le graphique final
  print(final_plot)
}

pieChartNationalite <- function(data) {
  ggplot(reponses_nationalite, aes(x = "", y = n, fill = nationalite_francaise)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    labs(fill = "Nationalité Française", 
         title = "Proportion des Répondants par Statut de Nationalité Française") +
    scale_fill_brewer(palette = "Set1") +
    theme_void() +
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  
}


stackedBarChartSexe <- function(data) {
  ggplot(data, aes(x = filiere, fill = sexe)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("Homme" = "#4477AA", "Femme" = "#EE6677", "Ne souhaite pas répondre" = "grey50")) +
    labs(x = "Filière", y = "Pourcentage", fill = "Sexe",
         title = "Répartition par Sexe au sein de chaque Filière") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}


# Fonction pour le diagramme en camembert des réponses par sexe
pieChartSexe <- function(data) {
  # Compter le nombre de réponses par sexe
  reponses_par_sexe <- data %>%
    group_by(sexe) %>%
    summarise(Nombre = n()) %>%
    ungroup()
  
  # Créer le diagramme en camembert
  pie_chart <- ggplot(reponses_par_sexe, aes(x = "", y = Nombre, fill = sexe)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    theme_void() +
    scale_fill_manual(values = c("Femme" = "#EE6677",  
                                 "Homme" = "#4477AA",  
                                 "Ne souhaite pas répondre" = "grey50")) +
    labs(title = "Répartition des Réponses par Genre",
         fill = "Sexe") +
    theme(legend.title = element_blank())
  
  # Créer le diagramme en barres empilées
  stacked_bar_chart <- stackedBarChartSexe(data)
  
  # Afficher les deux graphiques l'un en dessous de l'autre
  library(gridExtra)
  grid.arrange(pie_chart, stacked_bar_chart, nrow = 1)
  
}



# Fonction pour le boxplot de la rémunération annuelle brute par filière
boxplotFiliere <- function(data) {
  # le code pour le boxplot de la rémunération annuelle brute par filière ici
  ggplot(donnees_combinees_filtrees, aes(x = filiere, y = remuneration_annuelle_brute, fill = filiere)) +
    geom_boxplot(outlier.colour = "black", outlier.shape = 1) +
    scale_fill_manual(values = c("GBA" = "chartreuse3", "IG" = "deeppink1", "MEA" = "plum", "STE" = "deepskyblue3", 
                                 "MI"="salmon3", "SE-app"="skyblue3", "MSI-app"="lightsteelblue", "MAT"="lightseagreen", "EnR"="wheat1", "EGC-app"="sienna1")) +  # Définir les couleurs manuellement
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Incliner les étiquettes de l'axe x si elles sont trop longues
    labs(title = "Boxplot de la Rémunération Annuelle Brute par Filière",
         x = "Filière",
         y = "Rémunération Annuelle Brute") +
    theme_minimal() +
    theme(legend.position = "none")
  
}



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








# Run the application 
shinyApp(ui = ui, server = server)
















