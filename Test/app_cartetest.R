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
    leaflet() %>%
      setView(lng = -95, lat = 40, zoom = 3) %>%
      addTiles() %>%
      addPolygons(data = world_fertility, 
                  fillColor = ~pal(SP.DYN.TFRT.IN),
                  fillOpacity = 0.7, 
                  color = "#BDBDC3",
                  weight = 1,
                  popup = paste0("<b>Country:</b> ",world_fertility$region_un, "<br>",
                                 "<b>Fertility rate:</b> ", round(world_fertility$SP.DYN.TFRT.IN, 2))
                  ) %>%
      addLegend(pal = pal, 
                values = world_fertility$SP.DYN.TFRT.IN, 
                opacity = 0.7, 
                title = "Taux de fécondité")
  })
  

}

# Lancement de l'application shiny
shinyApp(ui = ui, server = server)
