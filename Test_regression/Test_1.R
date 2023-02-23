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
  
  # Sidebar contenant les filtres pour les variables explicatives
  sidebarLayout(
    sidebarPanel(
      selectInput("age_mere", "Âge de la mère", choices = unique(data$AgedelaMere), selected = NULL),
      selectInput("semaines_gestation", "Semaines de gestation", choices = unique(data$Nbsem), selected = NULL),
      selectInput("poids_mere", "Poids de la mère", choices = unique(data$PoidsMere), selected = NULL),
      
    ),
    
    # Graphique de la régression linéaire
    mainPanel(
      plotOutput("regression_plot")
    )
  )
)

# Définition de la fonction pour effectuer la régression linéaire
regression <- function(data, age_mere, semaines_gestation, poids_mere, sexe_bebe) {
  # Filtrage des données en fonction des variables explicatives sélectionnées
  filtered_data <- filter(data, AgedelaMere == age_mere, Nbsem == semaines_gestation, PoidsMere == poids_mere)
  
  # Régression linéaire multiple
  lm(PoidsBB ~ AgedelaMere + Nbsem + PoidsMere , data = filtered_data)
}

# Définition du serveur
server <- function(input, output) {
  
  # Graphique de la régression linéaire
  output$regression_plot <- renderPlot({
    # Calcul de la régression linéaire
    lm_model <- regression(data, input$age_mere, input$semaines_gestation, input$poids_mere)
    
    # Tracé du graphique avec les prédictions de la régression
    ggplot(data, aes(x = PoidsBB, y = PoidsBB)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
      geom_abline(intercept = lm_model$coefficients[1], slope = lm_model$coefficients[2], color = "red") +
      labs(title = "Régression linéaire du poids des bébés", x = "Poids des bébés (en grammes)", y = "Poids des bébés (en grammes)")
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)