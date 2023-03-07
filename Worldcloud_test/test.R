

######### WorldCloud #########
library(tidyverse)
#library(RColorBrewer)
library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(wordcloud)  # ce package permet la creation de wordcloud

library(shiny)
library(sf)
library(plotly)

# Data prenom

prenom <- read.csv("../data/dpt2021.csv", header= TRUE, sep=';')

prenom_annees <- subset(prenom, (preusuel != "_PRENOMS_RARES") & (annais != "XXXX"))
prenom_annees <- prenom_annees |>                                      
  group_by(preusuel, annais) |>                       
  summarise(nombre = sum(nombre))

prenom_annees$annais <- as.Date(prenom_annees$annais, format="%Y")

# Définir l'interface utilisateur
ui <- fluidPage(
  # Titre de la page
  # Zone de sélection des années
    title = "Prénoms au fur et à mesure des années", 
    sidebarLayout(
      sidebarPanel(
        textInput("prenom_bebe", "Prénom du bébé", value = "LAURENT")
      ),
      # Graphique des bébés 
      mainPanel(
        plotlyOutput("plot_bebe")
      )
    )
)


# Définir le serveur
server <- function(input, output) {
  

  
  # Sélectionner les données en fonction de l'année choisie
  output$plot_bebe <- renderPlotly({
    df <- prenom_annees |>  dplyr::filter(preusuel==input$prenom_bebe)
    ggplot(df)+
      aes(x = annais, y = nombre, color = "purple") + 
      geom_line(size = 1) + 
      scale_color_hue(direction = 1) +
      theme_minimal() + 
      scale_x_date(date_breaks = "20 years")
  })
  
  
  

}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
