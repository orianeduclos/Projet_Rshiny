#### Packages ####

library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)
library(rAmCharts)
library(highcharter)
library(plotly)

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



