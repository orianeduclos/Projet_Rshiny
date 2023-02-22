library(shiny)
library(leaflet)
library(dplyr)

# Importer les données

library(WDI)

fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2019, end = 2019)



#install.packages("rnaturalearth")
library(rnaturalearth)
library(tidyverse)
world <- ne_countries(scale = "medium", returnclass = "sf")
world_fertility <- left_join(world, fertility, by = c("iso_a3" = "iso3c"))
head(world_fertility)

pal <- colorNumeric(palette = "YlOrRd", domain = world_fertility$SP.DYN.TFRT.IN)

# Création de l'application shiny
ui <- fluidPage(
  leafletOutput("map")
  
)

server <- function(input, output) {
  
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
  

}

# Lancement de l'application shiny
shinyApp(ui = ui, server = server)
