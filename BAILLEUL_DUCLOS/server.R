library(shiny)
library(shinydashboard)
library(rAmCharts)
library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(highcharter)

##### Ouverture des bases de données #####

### Base de donnée BEBE ###
bebe <- read.table("../data/bebe.txt", header = TRUE, sep = ";")

## NA ##
manquant <- is.na(bebe)

# reparage par ligne 
coordmanquant <- which(manquant, arr.ind=TRUE)
coordmanquant[1:6,]
# eliminer doublon 
unique(coordmanquant[,1])

bool <- apply(is.na(bebe),1,any)
names(bool) <- NULL
which(bool)
bebepropre <- na.omit(bebe)

# transformation sexe en variable indicatrice
bebe <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))

### Base de données prénom ###

prenom <- read.csv("../data/dpt2021.csv", header= TRUE, sep=';')

### Base de données taux de fécondité ###

taux_fecondite <- read.csv("../data/taux_fecondite.csv", header= TRUE, sep=',')
taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)

### Base de données taux de fertilité ###

library(WDI)

fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2019, end = 2019)
dim(fertility)
world <- map_data("world")
dim(world)

# Jointure des données avec les données de la carte
fertility_map <- left_join(world, fertility, by = c("region" = "country"))
head(fertility_map)



##### Partie Server #####

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
#### Partie Pays ####    

  output$visu_pays <- DT::renderDataTable({
    datatable(taux_fecondite, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  output$summary_pays <- renderPrint({
    summary(taux_fecondite)
    
  })

  # Création de la carte leaflet
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles()
  })
  
  output$graphique_pays <- renderHighchart({
    hchart(
      taux_fecondite, "line", 
      hcaes(x = TIME, y =  Value, group = LOCATION)
    )
  })
  
#### Partie France #### 

  output$visu_france <- DT::renderDataTable({
    datatable(prenom, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  output$summary_france <- renderPrint({
    summary(prenom)
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })

  
#### Partie Maternité ####  
 
   output$visu_bebe <- DT::renderDataTable({
    datatable(bebe, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  output$summary_bebe <- renderPrint({
    summary(bebe)
  })
  

  
})
