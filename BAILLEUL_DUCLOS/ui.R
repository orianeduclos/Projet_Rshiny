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

# Define UI for application that draws a histogram

dashboardPage(
  dashboardHeader(title = "La natalité"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Dans les pays du monde", tabName = "pays", icon = icon("earth"),
        menuSubItem("Présentation BDD", tabName = "bddpays"),
        menuSubItem("À l'échelle mondiale", tabName = "monde"),
        menuSubItem("Pour seulement qql pays", tabName = "qqlpays")),
      menuItem(" En France", tabName = "france", icon = icon("location-dot"),
        menuSubItem("Présentation BDD", tabName = "bddfrance"), 
        menuSubItem("Traitement", tabName = "traitementfrance")),
      menuItem(" Dans une maternité", tabName = "mater", icon = icon("baby"),
        menuSubItem("Présentation BDD", tabName = "bddmater"), 
        menuSubItem("Traitement", tabName = "traitementmater"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "pays",
        
      ),
      
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
        tabName = "traitementpays",
      ),
      
      tabItem(
        tabName = "france",
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
          ),
        tabPanel(
          title = "Carte", 
          leafletOutput("map")
          )
        )
      ), 
      
      tabItem(
        tabName = "mater",
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
        tabName = "traitementmater"
      )
    )
  )
)
