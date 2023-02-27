######### WorldCloud #########
library(tidyverse)
library(RColorBrewer)
library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(wordcloud)  # ce package permet la creation de wordcloud
library(wordcloud2)

# Data prenom

data <- read.csv("../data/dpt2021.csv", header= TRUE, sep=';')


# Définir l'interface utilisateur
ui <- fluidPage(
  # Titre de la page
  titlePanel("World cloud des prénoms en France"),
  
  # Zone de sélection des années
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Sélectionnez une année :", choices = unique(data$annais)),
      actionButton("update", "Change")
     
      
    ),
    
    # Affichage du word cloud
    mainPanel(
      wordcloud2Output("wordcloud")
    )
  )
)


# Définir le serveur
server <- function(input, output) {
  
  # Sélectionner les données en fonction de l'année choisie
  prenom_data <- reactive({
    data[data$annais == input$year, ]
  })
  
  # Générer le word cloud
  output$wordcloud <- renderWordcloud2({
    ## Créer un corpus des prénoms sélectionnés
    docs <- Corpus(VectorSource(prenom_data()$preusuel))
    inspect(docs) # consulter l'interieur du document 
    # transformer caractere spé en espace 
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "_")
    inspect(docs)
    docs <- tm_map(docs, content_transformer(tolower)) # transformer texte en minuscule
    # Supprimer votre propre liste de mots non désirés
    inspect(docs)
    docs <- tm_map(docs, removeWords, c("prenoms rares")) 
    inspect(docs)
    
    # Supprimer les espaces vides supplémentaires
    docs <- tm_map(docs, stripWhitespace)
    
    ## Créer une matrice de termes-fréquences
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    
    # Générer le word cloud
    wordcloud2(d, size = 1.5, color = "random-light", backgroundColor = "white", rotateRatio = 0.5)
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)







