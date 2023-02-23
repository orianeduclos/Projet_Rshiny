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
      radioButtons(inputId = "idRadio", label = "Select one", choices = c("AgedelaMere", "Nbsem ", "PoidsMere"), selected = NULL)
    ),
    
    # Graphique de la régression linéaire
    mainPanel(
      plotOutput("regressionplot"), 
    )
  )
)


# Définition du serveur
server <- function(input, output) {
  
  output$ ({
    ggplot(bebe, PoidsBB ~ input$idRadio)+
      geom_point()
  })


}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
