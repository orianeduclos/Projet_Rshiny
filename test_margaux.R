#### Packages ####

library(tidyverse)
library(formattable)
library(dplyr)
library(ggplot2)

#### BDD ####

taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
#View(taux_fecondite)

bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
head(bebe)
#View(bebe)

#### Manipulation BDD ####

taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)

test <- taux_fecondite |> 
  
  pivot_wider(
    id_cols = LOCATION,
    names_from = TIME,
    values_from = Value
  )
#View(test)


#### Graphiques ####

# Faire un onglet avec tous les pays 
hchart(
  taux_fecondite, "line", 
  hcaes(x = TIME, y =  Value, group = LOCATION)
)

# Faire un onglet où on peut sélectionner les pays qu'on veut 

# Rajouter une colonne avec la moyenne de chaque pays 
# Voir si on peut pas rajouter des lignes avec la moyenne 

### Modifications pour l'affiache de la bdd bebe ###

# https://renkun-ken.github.io/formattable/ 
bebe$Peridurale <- case_when(
  bebe$Peridurale == "oui" ~ TRUE,
  bebe$Peridurale == "non" ~ FALSE)
#View(bebe)

formattable(bebe, list(
  Sexe = formatter("span", style = x ~ ifelse(x == "M", 
                                              style(color = "lightblue", font.weight = "bold"), 
                                              style(color = "lightpink", font.weight = "bold"))),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  Peridurale = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Oui", "Non"))
  )))

