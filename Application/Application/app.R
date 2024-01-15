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

data$type_combined <- ifelse(data$type_formation %in% c("Sous contrat d apprentissage", "Sous contrat d'apprentissage"),
                             "Sous contrat d'apprentissage", data$type_formation)

filtered_data <- data[!is.na(data$remuneration_prime) & data$type_combined != "", ]


filtered_data2 <- data[!is.na(data$remuneration_prime) & data$filiere_combined2 != ""& data$filiere_combined2 != "NA",]

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
                  checkboxGroupInput("filieres", "Sélectionner des filières", choices = unique(data$filiere_combined2)),
                  checkboxGroupInput("sexe", "Sélectionner le sexe", choices = c("Homme","Femme"))
                  
                ),
                mainPanel(
                  plotOutput("boxplot_filiere")
                )
              )
    ),
    tabPanel("Type de Formation", 
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("typesformation", "Sélectionner des types de formation", choices = unique(data$type_combined)),
                 checkboxGroupInput("sexes", "Sélectionner le sexe", choices = c("Homme","Femme"))
                 
               ),
               mainPanel(
                 plotOutput("boxplot_typeformation")
                 )
             )
    ),
    tabPanel("Obtention diplome", 
             h4("Diagrammes de boîte (box plots) et diagrammes de dispersion"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("datediplome", "Sélectionner des types de formation", choices = c("2015","2016","2017","2018","2019","2020","2021","2022")),
                 checkboxGroupInput("sexess", "Sélectionner le sexe", choices = c("Homme","Femme"))
                 
               ),
               mainPanel(
                 plotOutput("boxplot_datediplome")
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
  
}
shinyApp(ui = ui, server = server)

