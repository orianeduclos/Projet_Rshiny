#library(shiny)
library(colourpicker)
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
library(bslib)
library(plotly)
library(sf)
library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(wordcloud)  # ce package permet la creation de wordcloud
library(shinyWidgets)
library(sparkline) # pour affichage beau tableau 
library(reactable)
library(reactablefmtr)
library(corrplot) # affichage matrice de correlation
library(car)
library(rsconnect) # pour publier 
library(corrplot)

### Base de données taux de fécondité ###

taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)
taux_fecondite$LOCATION <- as.factor(taux_fecondite$LOCATION)
levels(taux_fecondite$LOCATION) <- c("Argentine", "Australie", "Autriche", "Belgique", "Bulgarie", "Brésil", "Canada", "Suisse", "Chili", "Chine", "Colombie", "Costa Rica", "Chypre", "République Tchèque", "Allemagne", "Danemark", "Espagne", "Estonie", "Union Européenne", "Finlande", "France", "Royaume-Uni", "Grèce", "Croatie", "Hongrie", "Indonésie", "Inde", "Irlande", "Islande", "Israël", "Italie",  "Japon", "Corée", "Lituanie", "Luxembourg", "Lettonie", "Mexique", "Malte", "Pays-Bas", "Norvège", "Nouvelle Zélande", "OAVG", "Pérou", "Pologne", "Portugal", "Roumanie", "Russie", "Arabie Saoudite", "Slovaquie", "Slovénie", "Suède", "Turquie", "États-Unis", "Afrique du Sud")       


### Base de donnée BEBE ###
bebe_ouverture <- read.table("data/bebe.txt", header = TRUE, sep = ";")

bebepropre <- na.omit(bebe_ouverture)

# transformation sexe en variable indicatrice
bebe <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))

bebe_sexe <- bebe |> 
  pivot_longer(c('AgedelaMere','Agedupere'), 
               names_to = "SEXEPARENT",
               values_to ="AGEPARENT") |> 
  mutate(Sexe_parent = case_when(SEXEPARENT=="AgedelaMere"~"Mere",
                                 SEXEPARENT=="Agedupere"~"Pere"))

#### Code pour la carte taux de fertilité dans le monde ####
# fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2017, end = 2019)
# 
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# world_fertility <- left_join(world, fertility, by = c("iso_a3" = "iso3c"))
# 
# pal <- colorNumeric(palette = "YlOrRd", domain = world_fertility$SP.DYN.TFRT.IN)

### BASE DE DONNEE PRENOM ###
prenom <- read.csv("data/dpt2021.csv", header= TRUE, sep=';')

prenom_annees <- subset(prenom, (preusuel != "_PRENOMS_RARES") & (annais != "XXXX"))
prenom_annees <- prenom_annees |>                                      
  group_by(preusuel, annais) |>                       
  summarise(nombre = sum(nombre))
prenom_annees$annais <- as.Date(prenom_annees$annais, format="%Y")


