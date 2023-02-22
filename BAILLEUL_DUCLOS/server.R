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
library(WDI) # package qui nous permet d'importer fertility
library(rnaturalearth) # package utilisé pour la carte 

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


#### Code pour la carte taux de fertilité dans le monde ####
fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2019, end = 2019)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_fertility <- left_join(world, fertility, by = c("iso_a3" = "iso3c"))
head(world_fertility)

pal <- colorNumeric(palette = "YlOrRd", domain = world_fertility$SP.DYN.TFRT.IN)


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
      setView(lng = -95, lat = 40, zoom = 3) %>%
      addTiles() |> 
      addPolygons(data = world_fertility, 
                  label = ~ world_fertility$name_sort,
                  opacity= 1,
                  dashArray = "2",
                  fillColor = ~pal(SP.DYN.TFRT.IN),
                  fillOpacity = 0.8, 
                  color = "#BDBDC3",
                  highlightOptions = highlightOptions(color = "#666", weight = 2, dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                  weight = 1,
                  popup = paste0("<b>Country:</b> ",world_fertility$name_sort, "<br>",
                                 "<b>Fertility rate:</b> ", round(world_fertility$SP.DYN.TFRT.IN, 2))
      ) |> 
      addLegend(pal = pal, 
                values = world_fertility$SP.DYN.TFRT.IN, 
                opacity = 0.7, 
                title = "Taux de fécondité") |> 
      # Ajout d'un rectangle à la main sur la france 
      addRectangles(
        lng1 = -5.11, lat1 = 52.10,
        lng2 = 10.92, lat2 = 40.25,
        color = "green",
        popup = "France",
        fill = FALSE)
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
