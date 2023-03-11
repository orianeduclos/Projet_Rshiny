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
library(lubridate)

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

prenom <- read.csv("data/dpt2021.csv", header= TRUE, sep=';')

prenom <- subset(prenom, (preusuel != "_PRENOMS_RARES") & (annais != "XXXX"))


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
prenom_dpt <- st_transform(prenom_dpt, crs = 4326)
pal <- colorNumeric(palette = "YlOrRd", domain = prenom_dpt$x)

leaflet(prenom_dpt$geometry) |> 
  addTiles() |> 
  setView(lng = France[1], lat = France[2], zoom = 6) |> 
  addPolygons(fillColor = ~pal(x),
              weight = 1)


prenom_graph <- prenom |> 
  pivot_wider(
    id_cols = preusuel,
    names_from = annais,
    values_from = nombre, 
    values_fn = sum, 
    values_fill = 0
  )

ggplot(prenom) + 
  aes(x = annais, y = nombre) + 
  geom_point()

output$evolution_globale <- renderAmCharts({
  nb_attentat_annee$Annee=as.POSIXct(nb_attentat_annee$Annee)
  amTimeSeries(nb_attentat_annee, 'Annee', "NB",color=input$color, export = TRUE)
})

output$plot_bebe <- renderPlotly({
  df <- prenom_annees |>  dplyr::filter(preusuel==input$prenom_bebe)
  p <- ggplot(df)+aes(x=annais,y=nombre)+
    geom_point() + 
    scale_x_date(date_breaks = "10 years")
  ggplotly(p)
})

summary(prenom)
head(prenom)


prenom_annees <- subset(prenom, (preusuel != "_PRENOMS_RARES") & (annais != "XXXX"))
prenom_annees <- prenom_annees |>                                      
  group_by(preusuel, annais) |>                       
  summarise(nombre = sum(nombre))

leaflet() |> 
  addTiles() |> 
  setView(lng = France[1], lat = France[2], zoom = 6) |> 
  addPolygons(data = prenom_dpt, 
              fillColor = ~pal(x),
              fillOpacity = 0.7, 
              color = "#BDBDC3",
              weight = 1)



