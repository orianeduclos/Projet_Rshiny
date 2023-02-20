etat_civil <- read.csv("data/FD_NAIS_2021.csv", header= TRUE, sep=';')
head(etat_civil)

bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
head(bebe)
dim(bebe)

taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
View(taux_fecondite)



#### Boxplot Age mere age pere ####

library(rAmCharts)
library(tidyverse)

amBoxplot(
  etat_civil$AGEPERE
)

age <- etat_civil |> pivot_longer(c('AGEMERE','AGEPERE'), 
                                  names_to = "SEXEPARENT",
                                  values_to ='AGEPARENT')
#View(age)

#amBoxplot(age$SEXEPARENTS~age$AGEPARENT)

ggplot(age,aes(x=SEXEPARENT, y= AGEPARENT, color = SEXEPARENT))+
  geom_boxplot()+ 
  stat_summary(fun.y = mean, geom='point', shape=23, size=4)+ 
  scale_color_manual(values=c('red','blue'))+
  scale_fill_manual(values=c('#CD5C5C','#87CEFA'))+
  theme_classic()

ggplot(bebe, aes(x=bebe$AgedelaMere))
  
##### Regression #####



##### WorldCloud #####




