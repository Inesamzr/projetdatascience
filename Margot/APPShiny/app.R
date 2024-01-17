library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringi)
library(forcats)

# Load your data outside the server function
data <- read.csv("merged_database.csv")
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

# Define UI
ui <- fluidPage(
  tags$style(HTML("
    body {
      font-family: 'Arial', sans-serif;
      background-color: #e6f7ff; /* Light Blue */
      margin: 0;
      padding: 0;
    }

    .shiny-tab-content {
      padding: 20px;
    }

    h1 {
      color: #333333;
    }

    /* Add CSS for buttons */
    .btn-default {
      background-color: #003366; /* Dark Blue */
      color: white;
      border: none;
      padding: 10px 20px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      margin: 4px 2px;
      cursor: pointer;
      border-radius: 4px;
    }

    /* Add CSS for aligned checkboxes */
    .checkbox-inline {
      display: inline-block;
      margin-right: 10px; /* Adjust the margin as needed */
    }

  ")),
  titlePanel("Rémunération Premier Emploi - Shiny App"),
  tabsetPanel(
    tabPanel("Secteur d'activité", 
             actionButton("selectAll1", "Sélectionner tout"),
             actionButton("deselectAll1", "Désélectionner tout"),
             checkboxGroupInput("genderInput", "Sélectionnez le sexe:", 
                                choices = c("Homme", "Femme"), selected = "Homme"),
             checkboxGroupInput("sectorInput", "Sélectionnez le secteur d'activité:",
                                choices = unique(data1$secteur_premiere_entreprise), selected = unique(data1$secteur_premiere_entreprise)),
             plotOutput("plot1")),
    tabPanel("Localisation géographique", 
             actionButton("selectAll2", "Sélectionner tout"),
             actionButton("deselectAll2", "Désélectionner tout"),
             checkboxGroupInput("localisationInput", "Sélectionnez la localisation:",
                                choices = unique(data2$localisation_premier_emploi), selected = unique(data2$localisation_premier_emploi)),
             checkboxGroupInput("countryInput", "Sélectionnez le pays:",
                                choices = unique(data2$pays_premier_emploi), selected = unique(data2$pays_premier_emploi)),
             plotOutput("plot2")),
    tabPanel("Filière et Date de Diplôme", 
             actionButton("selectAll3", "Sélectionner tout"),
             actionButton("deselectAll3", "Désélectionner tout"),
             checkboxGroupInput("filiereInput", "Sélectionnez la filière:",
                                choices = unique(data3$filiere), selected = unique(data3$filiere)),
             checkboxGroupInput("dateDiplomeInput", "Sélectionnez la date de diplôme:",
                                choices = unique(data3$date_diplome), selected = unique(data3$date_diplome)),
             plotOutput("plot3"))
  )
)

# Define Server
server <- function(input, output, session) {
  
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
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
