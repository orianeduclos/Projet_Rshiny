taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
View(taux_fecondite)

bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
head(bebe)

test <- taux_fecondite |> 
  pivot_wider()
