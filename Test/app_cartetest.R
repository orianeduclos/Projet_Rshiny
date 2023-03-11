library(shiny)
library(ggplot2)
library(dplyr)
library(sf)


if (!(require(jsonlite))) install.packages("jsonlite")
mygeocode <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}

# Importer les données

dpt <- read_sf("../BAILLEUL_DUCLOS/data/dpt")
prenom <- read.csv("../BAILLEUL_DUCLOS/data/dpt2021.csv", header= TRUE, sep=';')
prenom <- prenom |> 
  rename("CODE_DEPT" = "dpt")
prenom_dpt <- inner_join(prenom, dpt, by = c("CODE_DEPT"))

mygeocode("France")
France <- c(1.888334, 46.60335) 


prenom_dpt <- aggregate(prenom_dpt$nombre, by=list(preusuel = prenom_dpt$preusuel, CODE_DEPT = prenom_dpt$CODE_DEPT), FUN=sum)
prenom_dpt <- inner_join(prenom_dpt, dpt, by = c("CODE_DEPT"))
prenom_dpt <- sf::st_as_sf(prenom_dpt)
prenom_dpt <- st_transform(prenom_dpt, crs = 4326)

pal <- colorNumeric(palette = "YlOrRd", domain = prenom_dpt$x)


# Création de l'application shiny
ui <- fluidPage(
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        selectInput(inputId = "selection_bebe", label = "Choisissez un prénom", choices = unique(prenom_dpt$preusuel))
      ),
      # carte des bébé
      mainPanel(
        plotOutput("carte_bebe_dpt")
      )
    )
  )

server <- function(input, output) {
  
  # Création de la carte leaflet
  output$carte_bebe_dpt <- renderPlot({
    ggplot(prenom_dpt) + geom_sf()
  })
}

# Lancement de l'application shiny
shinyApp(ui = ui, server = server)
