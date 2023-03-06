# Chargement des packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


prenom <- read.csv("../data/dpt2021.csv", header= TRUE, sep=';')
prenom <- subset(prenom, (preusuel != "_PRENOMS_RARES") & (annais != "XXXX"))
#prenom <- aggregate(prenom$nombre, by=list(preusuel = prenom$preusuel, annais = prenom$annais), FUN=sum)
prenom <- prenom |>                                      
  group_by(preusuel, annais) |>                       
  summarise(nombre = sum(nombre))

# Création de l'application Shiny
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Régression linéaire du poids des bébés"),
  
  # Sidebar contenant les filtres pour les variables explicatives
  sidebarLayout(
    sidebarPanel(
      textInput("prenom_bebe", "Prénom du bébé", value = "LAURENT")
    ),
    
    # Graphique de la régression linéaire
    mainPanel(
      plotlyOutput("plot_bebe")
    )
  )
)


# Définition du serveur
server <- function(input, output) {
  
  # Graphique de la régression linéaire
  output$plot_bebe <- renderPlotly({
      df <- prenom |>  dplyr::filter(preusuel==input$prenom_bebe)
      p <- ggplot(df)+aes(x=annais,y=nombre)+
        geom_point() 
      ggplotly(p)
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)