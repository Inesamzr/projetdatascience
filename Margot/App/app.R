library(shiny)
library(ggplot2)
library(dplyr)
library(stringi)
library(forcats)

# Define UI
ui <- fluidPage(
  titlePanel("Analyse croisée"),
  
  # Define the sidebar with tabs
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Secteur d'activité", 
                 checkboxGroupInput("genderInput", "Sélectionnez le sexe:", 
                                    choices = c("Homme", "Femme"), selected = "Homme"),
                 selectInput("secteurInput", "Sélectionnez le secteur d'activité:",
                             choices = unique(data$secteur_premiere_entreprise))
        ),
        tabPanel("Localisation géographique",
                 selectInput("localisationInput", "Sélectionnez la localisation:",
                             choices = unique(data$localisation_premier_emploi))
        ),
        tabPanel("Filière et Date de Diplôme",
                 selectInput("filiereInput", "Sélectionnez la filière:",
                             choices = unique(data$filiere)),
                 selectInput("dateDiplomeInput", "Sélectionnez la date de diplôme:",
                             choices = unique(data$date_diplome))
        )
      )
    ),
    
    # Define the main panel
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data
  my_data <- read.csv("merged_database.csv")
  
  # Diagramme 1 - Rémunération en fonction du secteur d'activité
  output$plot1 <- renderPlot({
    data1 <- data %>%
      filter(!is.na(secteur_premiere_entreprise) & secteur_premiere_entreprise != "",
             !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
             !is.na(sexe) & sexe != "" & sexe %in% input$genderInput,
             secteur_premiere_entreprise %in% input$secteurInput)
    
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
    # Similar logic for filtering based on input$genderInput
    data2 <- data %>%
      filter(!is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
             !is.na(localisation_premier_emploi) & localisation_premier_emploi != "",
             localisation_premier_emploi %in% input$localisationInput)
    
    data2$localisation_premier_emploi <- tolower(iconv(data2$localisation_premier_emploi, to = "UTF-8", sub = "byte"))
    data2$localisation_premier_emploi <- stringi::stri_trans_general(data2$localisation_premier_emploi, "Latin-ASCII")
    data2$localisation_premier_emploi <- gsub("'", " ", data2$localisation_premier_emploi)
    
    data2 <- data2 %>%
      mutate(pays_premier_emploi = ifelse(pays_premier_emploi == "Je travaillais depuis la Belgique pour une entreprise Bangladaise/Allemande", "Belgique", pays_premier_emploi)) %>%
      mutate(pays_premier_emploi = ifelse(pays_premier_emploi %in% c("Danemark", "Danemark "), "Danemark", pays_premier_emploi))
    
    data2 <- data2 %>%
      mutate(pays_premier_emploi = ifelse(localisation_premier_emploi == "en france", "France", pays_premier_emploi))
    
    ggplot(data2, aes(x = localisation_premier_emploi, y = remuneration_annuelle_brute_avec_prime_premier_emploi, fill = pays_premier_emploi)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Rémunération Premier Emploi en fonction de la localisation géographique",
           x = "Localisation",
           y = "Rémunération moyenne") +
      theme_minimal()
  })
  
  # Diagramme 3 - Rémunération Premier Emploi en fonction de la Filière et de la Date de Diplôme
  output$plot3 <- renderPlot({
    # Similar logic for filtering based on input$genderInput
    data3 <- data %>%
      filter(!is.na(filiere) & filiere != "",
             !is.na(remuneration_annuelle_brute_avec_prime_premier_emploi) & remuneration_annuelle_brute_avec_prime_premier_emploi != "",
             !is.na(date_diplome) & date_diplome != "",
             filiere %in% input$filiereInput,
             date_diplome %in% input$dateDiplomeInput)
    
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
    
    ggplot(data3, aes(x = filiere, y = remuneration_annuelle_brute_avec_prime_premier_emploi, color = date_diplome)) +
      geom_point() +
      labs(title = "Rémunération Premier Emploi en fonction de la Filière et de la Date de Diplôme",
           x = "Filière",
           y = "Rémunération Annuelle Brute",
           color = "Date de Diplôme") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
