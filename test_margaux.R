library(tidyverse)

taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
#View(taux_fecondite)

bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
head(bebe)
#View(bebe)

taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)

test <- taux_fecondite |> 

  pivot_wider(
    id_cols = LOCATION,
    names_from = TIME,
    values_from = Value
  )
View(test)

ggplot(data = taux_fecondite, aes(x = TIME, y = Value, group = LOCATION, color = LOCATION)) +
  geom_line() + geom_point()
# Rajouter une colonne avec la moyenne de chaque pays 
# Voir si on peut pas rajouter des lignes avec la moyenne 
  

