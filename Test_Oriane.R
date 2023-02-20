
##### Base de donnée etat civil ####
etat_civil <- read.csv("data/FD_NAIS_2021.csv", header= TRUE, sep=';')
head(etat_civil)



###### Boxplot Age mere age pere ######

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



#### Base de donnée BEBE ####
bebe <- read.table("data/bebe.txt", header = TRUE, sep = ";")
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
bebenew <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))

###### Regression ######

bebenew$Nbsem <- as.numeric(bebenew$Nbsem)
bebenew$PoidsBB <- as.numeric(bebenew$PoidsBB)
bebenew$AgedelaMere <- as.numeric(bebenew$AgedelaMere)
bebenew$Sexe_indicatrice <- as.numeric(bebenew$Sexe_indicatrice)
reg1 <- lm(PoidsBB ~ TailleBB + Nbsem + AgedelaMere + Sexe_indicatrice, data=bebenew)
summary(reg1)
library(car)
avPlots(reg1)

myvars <- c("PoidsBB", "TailleBB","Nbsem", "AgedelaMere", "Sexe_indicatrice")
data_matcorr <- bebenew[myvars]
str(bebenew)
mcor <- round(cor(data_matcorr),2)

library(corrplot)
corrplot(mcor, method="circle")

col <- colorRampPalette(c("#f56767", "#d60909","#FFFFFF", "#b4c2f0","#3a14e0"))
corrplot(mcor, method="color", col = col(200), 
         type = "upper", order = "hclust",addCoef.col = "black", diag=FALSE,tl.col="black")
vif(reg1)
sqrt(vif(reg1))

# PAS DE COLIN2ARIT2 ENTRE VARIABLE 





ggplot(bebe, aes(x=bebe$AgedelaMere))



#### Base de donnée taux de fécondité ####
taux_fecondite <- read.csv("data/taux_fecondite.csv", header= TRUE, sep=',')
head(taux_fecondite)
View(taux_fecondite)




##### WorldCloud #####




