library(shiny)
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

### Base de données taux de fécondité ###

taux_fecondite <- read.csv("../data/taux_fecondite.csv", header= TRUE, sep=',')
taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)
taux_fecondite$LOCATION <- as.factor(taux_fecondite$LOCATION)
levels(taux_fecondite$LOCATION) <- c("Argentine", "Australie", "Autriche", "Belgique", "Bulgarie", "Brésil", "Canada", "Suisse", "Chili", "Chine", "Colombie", "Costa Rica", "Chypre", "République Tchèque", "Allemagne", "Danemark", "Espagne", "Estonie", "Union Européenne", "Finlande", "France", "Royaume-Uni", "Grèce", "Croatie", "Hongrie", "Indonésie", "Inde", "Irlande", "Islande", "Israël", "Italie",  "Japon", "Corée", "Lituanie", "Luxembourg", "Lettonie", "Mexique", "Malte", "Pays-Bas", "Norvège", "Nouvelle Zélande", "OAVG", "Pérou", "Pologne", "Portugal", "Roumanie", "Russie", "Arabie Saoudite", "Slovaquie", "Slovénie", "Suède", "Turquie", "États-Unis", "Afrique du Sud")       


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

#### Code pour la carte taux de fertilité dans le monde ####
fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2017, end = 2019)


world <- ne_countries(scale = "medium", returnclass = "sf")
world_fertility <- left_join(world, fertility, by = c("iso_a3" = "iso3c"))
head(world_fertility)

# Define UI for application that draws a histogram

dashboardPage(skin='purple',
  dashboardHeader(title = "La natalité 👶", 
                  dropdownMenu(type="message", messageItem(from = "Margaux et Oriane", message="Bienvenue sur notre application 👶",icon=icon("envelope-open"), time = "Now"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("door-open")),
      menuItem(" Dans les pays du monde", tabName = "pays", icon = icon("earth"),
        menuSubItem("Présentation BDD", tabName = "bddpays"),
        menuSubItem("À l'échelle mondiale", tabName = "monde"),
        menuSubItem("Zoom sur les continents", tabName = "qqlpays")),
      menuItem(" En France", tabName = "france", icon = icon("location-dot"),
        menuSubItem("Présentation BDD", tabName = "bddfrance"), 
        menuSubItem("Traitement", tabName = "traitementfrance")),
      menuItem(" Dans une maternité", tabName = "mater", icon = icon("baby"),
        menuSubItem("Présentation BDD", tabName = "bddmater"), 
        menuSubItem("Régression", tabName = "regressionmater"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "bddpays",
        tabsetPanel(
          tabPanel(
            title = "Présentation de la base de données", 
            dataTableOutput("visu_pays")
          ), 
          tabPanel(
            title = "Summary", 
            verbatimTextOutput("summary_pays")
          ),
        )
      ),
      
      tabItem(
        tabName = "monde",
        tabsetPanel(
          tabPanel(
            title = "Carte", 
            sidebarPanel(
              selectInput(inputId = "Year", label = "year", choices = unique(world_fertility$year))
            ),
            leafletOutput("map")
          ), 
          tabPanel(
            title = "Graphique sur les pays", 
            highchartOutput("graphique_pays")
          ), 
          tabPanel(
            title = "Graphique sur un pays", 
            sidebarPanel(
              selectInput(inputId = "pays_seul", label = "Choisissez un pays", choices = unique(taux_fecondite$LOCATION))
            ),
            plotOutput("graphique_pays_indiv")
          )
        )
      ),
      
      tabItem(
        tabName = "bddfrance", 
        
        tabsetPanel(
          tabPanel(
            title = "Présentation de la base de données", 
            dataTableOutput("visu_france")
          ), 
          tabPanel(
            title = "Summary", 
            verbatimTextOutput("summary_france")
          )
        )
      ),
     
      
      tabItem(
        tabName = "traitementfrance",
        
        tabsetPanel(
          # Create a "Word cloud" tab
          tabPanel(
            title = "Word cloud",
        #### world cloud ####
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            selectInput("selection", "Choissisez une année:",
                        choices = c("année")),
            
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 50, value = 15),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  max = 300,  value = 100)
          ),
          
          # Show Word Cloud
          mainPanel(
            plotOutput("wordcloud")
          )
        )
          )
        )
      ), 

      tabItem(
        tabName = "bddmater", 
        
        tabsetPanel(
          tabPanel(
            title = "Présentation de la base de données", 
            dataTableOutput("visu_bebe")
          ), 
          tabPanel(
            title = "Summary", 
            verbatimTextOutput("summary_bebe")
          )
        )
      ), 
      
      tabItem(
        tabName = "regressionmater", 
        tabsetPanel(
          tabPanel(
            title = "Régression sortie", 
            sidebarLayout(
              sidebarPanel(
                p("Select the inputs for the Dependent Variable"),
                selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("PoidsBB", "TailleBB")),
                p("Select the inputs for the Independent Variable"),
                checkboxGroupInput(inputId = "IndVar", label = "Independent Variables", choices = colnames(bebe), selected = "TailleBB")
              ),
              mainPanel(
                verbatimTextOutput(outputId = "RegSum"),
                verbatimTextOutput(outputId = "IndPrint"),
                verbatimTextOutput(outputId = "DepPrint"),
              )
            )
          ), 
          tabPanel(
            title = "Régression graphique", 
            plotOutput("rl")
          )
        )
      )
    )
  )
)
