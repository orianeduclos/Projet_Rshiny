# Chargement des packages
library(shiny)
library(ggplot2)
library(dplyr)


#### Base de donnée BEBE
bebe <- read.csv("../../data/bebe.txt", sep=";")
head(bebe)
#View(bebe)
dim(bebe)
summary(bebe)

###### NA ######
mean(bebe$TailleBB, na.rm=TRUE)
manquant <- is.na(bebe)
dim(manquant)
sum(manquant)

# reparage par ligne 
coordmanquant <- which(manquant, arr.ind=TRUE)
coordmanquant[1:6,]
# eliminer doublon 
unique(coordmanquant[,1])

bool <- apply(is.na(bebe),1,any)
names(bool) <- NULL
which(bool)

library(tidyverse)
bebepropre <- na.omit(bebe)

# transformation sexe en variable indicatrice
data <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))


# Define server logic required to draw a histogram
function(input, output, session) {

  lm1 <- reactive({
    lm(reformulate(input$IndVar, input$DepVar), data = data)
    })
  
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  output$rl <- renderPlot({
    ggplot(data, aes(x = PoidsBB, y = input$test)) + 
      geom_point()+
      geom_smooth(method="lm")
  })
}
