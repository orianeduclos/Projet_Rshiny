#### Packages ####

library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)
library(rAmCharts)
library(highcharter)
library(plotly)
library(leaflet)
library(jsonlite)
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

#### BDD ####

taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
#View(taux_fecondite)

bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
head(bebe)
#View(bebe)

dpt2021 <- read.table("data/dpt2021.csv", header = TRUE, sep = ";")

#### Manipulation BDD ####

taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)

test <- taux_fecondite |> 
  
  pivot_wider(
    id_cols = LOCATION,
    names_from = TIME,
    values_from = Value
  )

taux_fecondite$LOCATION <- as.factor(taux_fecondite$LOCATION)
levels(taux_fecondite$LOCATION)
levels(taux_fecondite$LOCATION) <- c("Argentine", "Australie", "Autriche", "Belgique", "Bulgarie", "Brésil", "Canada", "Suisse", "Chili", "Chine", "Colombie", "Costa Rica", "Chypre", "République Tchèque", "Allemagne", "Danemark", "Espagne", "Estonie", "Union Européenne", "Finlande", "France", "Royaume-Uni", "Grèce", "Croatie", "Hongrie", "Indonésie", "Inde", "Irlande", "Islande", "Israël", "Italie",  "Japon", "Corée", "Lituanie", "Luxembourg", "Lettonie", "Mexique", "Malte", "Pays-Bas", "Norvège", "Nouvelle Zélande", "OAVG", "Pérou", "Pologne", "Portugal", "Roumanie", "Russie", "Arabie Saoudite", "Slovaquie", "Slovénie", "Suède", "Turquie", "États-Unis", "Afrique du Sud")       

prenom <- read.csv("../data/dpt2021.csv", header= TRUE, sep=';')

#### Graphiques ####

# Faire un onglet avec tous les pays 
hchart(
  taux_fecondite, "line", 
  hcaes(x = TIME, y =  Value, group = LOCATION)
)

hchart(
  taux_fecondite, "line", 
  hcaes(x = TIME, y =  Value, group = LOCATION)
)

df <- reactive({
  pays <- new.env()
  getSymbols(
    input$choixpays, 
    env = pays,
    do.call(merge, lapply(pays, ))
  )
})

output$graphpaysseul <- renderPlot({
  ggplot(df(), aes(x = TIME, y =  Value, group = LOCATION))
})

data_base1 <- data_base %>% dplyr::filter(Equipe==input$varclub)
data_base$EfficaciteTirEquipe[which(data_base$Equipe==input$varclub)]
ggplot(data_base1)+aes(x=Annee,y=EfficaciteTirEquipe)+
  geom_point()+geom_smooth()+theme_bw()+
  ggtitle(paste("Evolution de l'efficacité de Tirs de l'équipe",input$varclub))+theme(plot.title = element_text(hjust = 0.45))


# The dynamically-generated user panel


# Faire un onglet où on peut sélectionner les pays qu'on veut 

# Rajouter une colonne avec la moyenne de chaque pays 
# Voir si on peut pas rajouter des lignes avec la moyenne 

### Modifications pour l'affiache de la bdd bebe ###

# https://renkun-ken.github.io/formattable/ 

bebe$Peridurale <- case_when(
  bebe$Peridurale == "oui" ~ TRUE,
  bebe$Peridurale == "non" ~ FALSE)

formattable(bebe, list(
  Sexe = formatter("span", style = x ~ ifelse(x == "M", 
                                              style(color = "lightblue", font.weight = "bold"), 
                                              style(color = "lightpink", font.weight = "bold"))),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  Peridurale = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Oui", "Non"))))) |> 
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE),
               rownames = FALSE)

p1 <- ggplot(bebe) + aes(x = ModeAccouc) + geom_histogram(stat="count")
ggplotly(p1)
  
p2 <- ggplot(bebe) + aes(x = Operant) + geom_histogram(stat="count")
ggplotly(p2)

### Création de la carte ###

dpt <- read_sf("../data/dpt")
prenom <- prenom |> 
  rename("CODE_DEPT" = "dpt")
prenom_dpt <- inner_join(prenom, dpt, by = c("CODE_DEPT"))

mygeocode("France")
France <- c(1.888334, 46.60335) 


prenom_dpt <- aggregate(prenom_dpt$nombre, by=list(preusuel = prenom_dpt$preusuel, CODE_DEPT = prenom_dpt$CODE_DEPT), FUN=sum)
prenom_dpt <- inner_join(prenom_dpt, dpt, by = c("CODE_DEPT"))
prenom_dpt <- sf::st_as_sf(prenom_dpt)

pal <- colorNumeric(palette = "YlOrRd", domain = prneom_dpt_test2$x)

leaflet() |> 
  addProviderTiles(providers$Esri.WorldImagery) |> 
  setView(lng = France[1], lat = France[2], zoom = 6) |> 
  addPolygons(data = prenom_dpt, 
              fillColor = ~pal(x),
              fillOpacity = 0.7, 
              color = "#BDBDC3",
              weight = 1)

    addPolygons(data = fertility_reactive(), 
                label = ~ fertility_reactive()$name_sort,
                opacity= 1,
                dashArray = "2",
                fillColor = ~pal(SP.DYN.TFRT.IN),
                fillOpacity = 0.8, 
                color = "#BDBDC3",
                highlightOptions = highlightOptions(color = "#666", weight = 2, dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                weight = 1,
                popup = paste0("<b>Country:</b> ",fertility_reactive()$name_sort, "<br>",
                               "<b>Fertility rate:</b> ", round(fertility_reactive()$SP.DYN.TFRT.IN, 2))
    ) |> 
    addLegend(pal = pal, 
              values = fertility_reactive()$SP.DYN.TFRT.IN, 
              opacity = 0.7, 
              title = "Taux de fécondité") 
  





  addPolygons(data = world_fertility, 
              fillColor = ~pal(SP.DYN.TFRT.IN),
              fillOpacity = 0.7, 
              color = "#BDBDC3",
              weight = 1) %>%
  addLegend(pal = pal, 
            values = world_fertility$SP.DYN.TFRT.IN, 
            opacity = 0.7, 
            title = "Taux de fécondité")

  
  fertility_reactive <- reactive({
    subset(
      world_fertility, 
      year == input$Year
    )
  })
(data = dpt2,color=~pal(t_prev),fillOpacity = 0.6, 
  stroke = TRUE,weight=1,
  popup=~paste(as.character(NOM_DEPT),as.character(t_prev),sep=" : "),
  highlightOptions = highlightOptions(color = "black", weight = 3,bringToFront = TRUE))
  
  
  
  
  
  
  
  nouvelle_palette <- colorPalette(c("#E9D1F2", "#C86DF2", "#B87AD3"))
