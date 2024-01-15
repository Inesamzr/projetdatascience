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

data <- read.csv("merged_database.csv", header = TRUE)

ui <- fluidPage(
  
  titlePanel("Analyses Formation App"),
  
  # Onglets
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
    
    tabPanel("Diagramme en boîte", 
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             selectizeInput("selected_filiere", "Sélectionner une filière pour le boxplot", choices = unique(data$filiere_combined3)),
             plotOutput("boxplot_filiere"),
             plotOutput("boxplot_typeformation"),
             plotOutput("boxplot_datediplome")
    )
  )

)

server <- function(input, output) {
  
  output$histogram_filiere <- renderPlot({
    data$filiere_combined2[data$filiere %in% c("Eau et Génie Civil (EGC - apprentissage)", "Eau et GÈnie Civil (EGC − apprentissage)")] <- "Eau et Génie Civil (EGC − apprentissage)"
    data$filiere_combined2[data$filiere %in% c("Génie Biologique et Agroalimentaires (GBA)", "GÈnie Biologique et Agroalimentaires (GBA)")] <- "Génie Biologique et Agroalimentaires (GBA)"
    data$filiere_combined2[data$filiere %in% c("Matériaux (MAT)", "MatÈriaux (MAT)")] <- "Matériaux (MAT)"
    data$filiere_combined2[data$filiere %in% c("Mécanique et Interactions (MI)", "MÈcanique et Interactions (MI)")] <- "Mécanique et Interactions (MI)"
    data$filiere_combined2[data$filiere %in% c("Mécanique Structures Industrielles (MSI − apprentissage)", "MÈcanique Structures Industrielles (MSI − apprentissage)")] <- "Mécanique Structures Industrielles (MSI − apprentissage)"
    data$filiere_combined2[data$filiere %in% c("Microélectronique Et Automatique (MEA)", "MicroÈlectronique Et Automatique (MEA)")] <- "Microélectronique Et Automatique (MEA)"
    data$filiere_combined2[data$filiere %in% c("Sciences et Technologies de l Eau (STE)", "Sciences et Technologies de l'Eau (STE)")] <- "Sciences et Technologies de l'Eau (STE)"
    data$filiere_combined2[data$filiere %in% c ("Energétique - énergies Renouvelables (EnR)")] <- "Energétique - énergies Renouvelables (EnR)"
    data$filiere_combined2[data$filiere %in% c ("Informatique et Gestion (IG)")] <- "Informatique et Gestion (IG)"
    data$filiere_combined2[data$filiere %in% c("Systèmes Embarqués (SE - apprentissage)", "Systèmes EmbarquÈs (SE - apprentissage)")] <- "Systèmes Embarqués (SE - apprentissage)"
    data$filiere_combined2[data$filiere %in% c("Mécanique Structures Industrielles (MSI - apprentissage)", "MÈcanique Structures Industrielles (MSI - apprentissage)")] <- "Mécanique Structures Industrielles (MSI - apprentissage)"
    
    filtered_data2 <- data[!is.na(data$remuneration_prime) & data$filiere_combined2 != "",]
  
    
    
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
    data$type_combined <- ifelse(data$type_formation %in% c("Sous contrat d apprentissage", "Sous contrat d'apprentissage"),
                                 "Sous contrat d'apprentissage", data$type_formation)
    
    filtered_data <- data[!is.na(data$remuneration_prime) & data$type_combined != "", ]
    
    histogram <- ggplot(filtered_data, aes(x = type_combined, y = remuneration_prime, fill = type_combined)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Répartition des salaires par type de formation",
           x = "Type de Formation",
           y = "Rémunération Prime") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() 
    print(histogram)
  })
  
  # Convertir date_diplome en facteur
  data$date_diplome <- as.factor(data$date_diplome)
  
  # Puis utiliser le code pour le graphique
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
  
  

  output$boxplot_filiere <- renderPlot({
    selected_filiere <- input$selected_filiere
    
    data$filiere_combined3[data$filiere %in% c("Eau et Génie Civil (EGC - apprentissage)", "Eau et GÈnie Civil (EGC − apprentissage)")] <- "Eau et Génie Civil (EGC − apprentissage)"
    data$filiere_combined3[data$filiere %in% c("Génie Biologique et Agroalimentaires (GBA)", "GÈnie Biologique et Agroalimentaires (GBA)")] <- "Génie Biologique et Agroalimentaires (GBA)"
    data$filiere_combined3[data$filiere %in% c("Matériaux (MAT)", "MatÈriaux (MAT)")] <- "Matériaux (MAT)"
    data$filiere_combined3[data$filiere %in% c("Mécanique et Interactions (MI)", "MÈcanique et Interactions (MI)")] <- "Mécanique et Interactions (MI)"
    data$filiere_combined3[data$filiere %in% c("Mécanique Structures Industrielles (MSI − apprentissage)", "MÈcanique Structures Industrielles (MSI − apprentissage)")] <- "Mécanique Structures Industrielles (MSI − apprentissage)"
    data$filiere_combined3[data$filiere %in% c("Microélectronique Et Automatique (MEA)", "MicroÈlectronique Et Automatique (MEA)")] <- "Microélectronique Et Automatique (MEA)"
    data$filiere_combined3[data$filiere %in% c("Sciences et Technologies de l Eau (STE)", "Sciences et Technologies de l'Eau (STE)")] <- "Sciences et Technologies de l'Eau (STE)"
    data$filiere_combined3[data$filiere %in% c ("Energétique - énergies Renouvelables (EnR)")] <- "Energétique - énergies Renouvelables (EnR)"
    data$filiere_combined3[data$filiere %in% c ("Informatique et Gestion (IG)")] <- "Informatique et Gestion (IG)"
    data$filiere_combined3[data$filiere %in% c("Systèmes Embarqués (SE - apprentissage)", "Systèmes EmbarquÈs (SE - apprentissage)")] <- "Systèmes Embarqués (SE - apprentissage)"
    data$filiere_combined3[data$filiere %in% c("Mécanique Structures Industrielles (MSI - apprentissage)", "MÈcanique Structures Industrielles (MSI - apprentissage)")] <- "Mécanique Structures Industrielles (MSI - apprentissage)"
    

    json_data <- toJSON(list(filiere_combined3 = filtered_data3$filiere_combined3, remuneration_prime = filtered_data3$remuneration_prime))
    
    ggplot(fromJSON(json_data), aes(x = factor(filiere_combined3), y = remuneration_prime)) +
      geom_boxplot() +
      labs(x = "Filière", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime par la filière") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  
  output$boxplot_typeformation <- renderPlot({
    ggplot(data, aes(x = factor(type_combined), y = remuneration_prime)) +
      geom_boxplot() +
      labs(x = "Type de Formation", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime par type de formation") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$boxplot_datediplome <- renderPlot({
    ggplot(data, aes(x = date_diplome, y = remuneration_prime)) +
      geom_boxplot() +
      labs(x = "Date d'obtention du diplôme", y = "Rémunération Prime") +
      ggtitle("Variation de la rémunération Prime par date d'obtention du diplôme") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
}
shinyApp(ui = ui, server = server)

