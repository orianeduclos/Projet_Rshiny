library(shiny)
library(shinydashboard)
library(rAmCharts)
library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)

# Ouverture des bases de données

bebe <- read.table("../data/bebe.txt", header = TRUE, sep = ";")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$visu_bebe <- renderPrint({
    
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
                             x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Oui", "Non")))))
  })
  
  output$summary_bebe <- renderPrint({
    summary(bebe)
  })
  
})
