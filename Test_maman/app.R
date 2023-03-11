library(shiny)


bebe <- read.table("BAILLEUL_DUCLOS/data/bebe.txt", header = TRUE, sep = ";")
bebe$age_class <- cut(bebe$AgedelaMere, breaks = c(0, 20, 25, 30, 35, 39, Inf), labels = c("lt20", "20to24", "25to29", "30to34", "35to39", "ge40"))



# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Profil moyen de la maman à l'accouchement"),
  sidebarLayout(
    sidebarPanel(
      h4("Sélectionnez les variables pour afficher les résultats:"),
      selectInput("age", "Âge de la maman :", choices = c("Tous" = "all", "Moins de 20 ans" = "lt20", "20-24 ans" = "20to24", "25-29 ans" = "25to29", "30-34 ans" = "30to34", "35-39 ans" = "35to39", "40 ans et plus" = "ge40"), selected = "all"),
    ),
    mainPanel(
      plotOutput("graph")
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  

  
  # Fonction pour filtrer les données en fonction des sélections de l'utilisateur
  filter_data <- reactive({
    if (input$age == "all") {
      return(bebe)
    } else {
      filtered_data <- bebe
      if (input$age != "all") {
        filtered_data <- filtered_data[filtered_data$age_class == input$age,]
      }
      
      
      return(filtered_data)
    }
  })
  
  # Fonction pour créer le graphique en fonction des données filtrées
  output$graph <- renderPlot({
    data <- filter_data()
    ggplot(data, aes(x = PoidsBB)) + 
      geom_density(fill = "blue", alpha = 0.3) +
      xlab("Poids du bébé à la naissance (en onces)") + 
      ylab("Densité") + 
      ggtitle("Profil moyen de la maman à l'accouchement")
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

