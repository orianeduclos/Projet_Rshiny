#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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

# Define UI for application that draws a histogram
fluidPage(

  headerPanel("Regression and Time Series Analysis"), 
  sidebarPanel(
    p("Select the inputs for the Dependent Variable"),
    selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("PoidsBB")),
    p("Select the inputs for the Independent Variable"),
    checkboxGroupInput(inputId = "IndVar", label = "Independent Variables", choices = colnames(data), selected = "TailleBB"), 
    p("Test"),
    selectInput(inputId = "test", label = "test", choices = list("TailleBB", "PoidsMere"))
  ),
  mainPanel(
    verbatimTextOutput(outputId = "RegSum"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = "DepPrint"),
    plotOutput("rl")
  )
)
