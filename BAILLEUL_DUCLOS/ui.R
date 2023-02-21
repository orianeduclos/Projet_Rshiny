library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram

dashboardPage(
  dashboardHeader(title = "La natalité"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dans les pays du monde", tabName = "pays",
        menuSubItem("À l'échelle mondiale", tabName = "monde"),
        menuSubItem("Pour seulement qql pays", tabName = "qqlpays")),
      menuItem("En France", tabName = "france"),
      menuItem("Dans une maternité", tabName = "mater")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "pays",
      ), 
      
      
     
      tabItem(
        tabName = "france",
        
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
            plotOutput("plot")
          )
        )
          ),
        tabPanel(
          title = "BLABLA")
        )
      )
        
      ), 
      
      tabItem(
        tabName = "mater", 
        
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
      )
    )
  )
