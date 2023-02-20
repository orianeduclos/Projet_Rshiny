library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)

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
        tabName = "pays"
      ), 
      
      tabItem(
        tabName = "france",
      ), 
      
      tabItem(
        tabName = "mater"
      )
    )
  )
)