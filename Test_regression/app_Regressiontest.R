# Chargement des packages
library(shiny)
library(ggplot2)
library(dplyr)


#### Base de donnée BEBE
bebe <- read.csv("../data/bebe.txt", sep=";")
head(bebe)
#View(bebe)
dim(bebe)
summary(bebe)

###### NA ######
mean(bebe$TailleBB, na.rm=TRUE)
manquant <- is.na(bebe)
dim(manquant)
sum(manquant)

# reparage par ligne 
coordmanquant <- which(manquant, arr.ind=TRUE)
coordmanquant[1:6,]
# eliminer doublon 
unique(coordmanquant[,1])

bool <- apply(is.na(bebe),1,any)
names(bool) <- NULL
which(bool)

library(tidyverse)
bebepropre <- na.omit(bebe)

# transformation sexe en variable indicatrice
data <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))

# Création de l'application Shiny
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Régression linéaire du poids des bébés"),
  
  # Sidebar contenant les filtres pour la variable explicative
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable explicative", choices = c("AgedelaMere", "Nbsem ", "PoidsMere"), selected = NULL)
    ),
    
    # Graphique de la régression linéaire
    mainPanel(
      plotOutput("regression_plot")
    )
  )
)

# Définition de la fonction pour effectuer la régression linéaire
regression <- function(data, variable) {
  # Régression linéaire simple
  lm(PoidsBB ~ !!as.name(variable), data = data)
}

# Définition du serveur
server <- function(input, output) {
  
  # Graphique de la régression linéaire
  output$regression_plot <- renderPlot({
    # Calcul de la régression linéaire
    lm_model <- regression(data, input$variable)
    
    # Tracé du graphique avec les prédictions de la régression
    ggplot(data, aes(x = !!as.name(input$variable), y = weight)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
      geom_abline(intercept = lm_model$coefficients[1], slope = lm_model$coefficients[2], color = "red") +
      labs(title = paste("Régression linéaire du poids des bébés en fonction de", input$variable), x = input$variable, y = "Poids des bébés (en grammes)")
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
