##### Base de donnée prenom #####
prenom <- read.csv("data/dpt2021.csv", header= TRUE, sep=';')
View(prenom)


######### WorldCloud #########
library(tidyverse)
library(RColorBrewer)
library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(wordcloud)  # ce package permet la creation de wordcloud
library(wordcloud2)
prenom <- prenom[prenom$annais=='2020',]
head(prenom)

docs <- Corpus(VectorSource(prenom$preusuel))
inspect(docs) # consulter l'interieur du document 
# transformer caractere spé en espace 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "_")
inspect(docs)
docs <- tm_map(docs, content_transformer(tolower)) # transformer texte en minuscule
# Supprimer votre propre liste de mots non désirés
inspect(docs)
docs <- tm_map(docs, removeWords, c("prenoms rares")) 
inspect(docs)

# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)

# construction matrice 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
head(d, 10)

#generer le nuage de mots

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 50,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



############## VISU EN FRANCE AVEC ETAT CIVIL ##################

library(rAmCharts)



#### Base de donnée BEBE ####
bebe <- read.table("BAILLEUL_DUCLOS/data/bebe.txt", header = TRUE, sep = ";")
head(bebe)

bebepropre <- na.omit(bebe)

# transformation sexe en variable indicatrice
bebe <- bebepropre |> 
  mutate(Sexe_indicatrice = case_when(Sexe== "M" ~ "1",
                                      Sexe=="F"~ "0" ))


############## VISU EN FRANCE AVEC ETAT CIVIL ##################


################## VISU BEBE ###################
#(bebe$AgedelaMere)

bebe_sexe <- bebe |> 
  pivot_longer(c('AgedelaMere','Agedupere'), 
                                  names_to = "SEXEPARENT",
                                  values_to ="AGEPARENT") |> 
  mutate(Sexe_parent = case_when(SEXEPARENT=="AgedelaMere"~"Mere",
                                  SEXEPARENT=="Agedupere"~"Pere"))





amBoxplot(AGEPARENT ~ Sexe_parent , col = "pink", data =bebe, ylab="Age du parent", main= "Boxplot de l'age du parent en fonction du sexe")

colnames(bebe)

amAngularGauge(x = round(mean(bebe$Nbsem)), main= "Nombre de semaine de gestation moyenne") |> 
  amOptions(export = TRUE, exportFormat = "JPG")



Mode_accouchement <- as.data.frame(table(bebe$ModeAccouc))
colnames(Mode_accouchement) <- c("label","value")
amPie(data=Mode_accouchement, main="Mode d'accouchement")
Mode_travail <- as.data.frame(table(bebe$ModeTravai))
colnames(Mode_travail) <- c("label","value")
amBarplot(x = "label", y = "value", data = Mode_travail, horiz = TRUE)

#### Profit moyen de la maman ####



### Belle table avec boxplot en bas de la page 
library(sparkline)

library(sparkline)
library(reactable)
library(reactablefmtr)
bebenew %>% 
  reactable( theme = journal(),
             
    defaultPageSize = 5,
    defaultColDef = colDef(cell = data_bars(., fill_color = viridis::magma(5), text_position = "inside-end"),footer = function(values) {
      if (!is.numeric(values)) return()
      sparkline(values, type = "box", width = 100, height = 30)
    }))

colnames(bebenew)

c("Nbsem","Sexe","PoidsBB","TailleBB","PoidsPlacenta","Operant","JourNaiss","SitMat","AgedelaMere","NaissMere","TailMere","PoidsMere","Agedupere","NaisPere","TailPere","PoidsPere","NbGrossess","NbEnfants","NbIVG","NbFC","TypeAllait","ModeAccouc","ModeTravai","Peridurale","DureeTrava","IMCMere","PoidsQuart","Sexe_indicatrice")



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
for (i in myvars) {
  bebenew[[i]] <- as.numeric(bebenew[[i]])
}
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

#### Carte taux de fédondité monde #####
install.packages("WDI")
library(WDI)

fertility <- WDI(indicator = "SP.DYN.TFRT.IN", start = 2019, end = 2019)


#install.packages("rnaturalearth")
library(rnaturalearth)
library(tidyverse)
world <- ne_countries(scale = "medium", returnclass = "sf")
View(world)
world_fertility <- left_join(world, fertility, by = c("iso_a3" = "iso3c"))
View(world_fertility)

cat(mean(world_fertility$SP.DYN.TFRT.IN, na.rm=TRUE))

### Affichage en dynamique ###

library(sparkline)
library(reactable)
world_fertility %>% 
  reactable(
    defaultPageSize = 5,
    defaultColDef = colDef(footer = function(values) {
      if (!is.numeric(values)) return()
      sparkline(values, type = "box", width = 100, height = 30)
    })
  )



library(leaflet)

pal <- colorNumeric(palette = "YlOrRd", domain = world_fertility$SP.DYN.TFRT.IN)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = world_fertility, 
              fillColor = ~pal(SP.DYN.TFRT.IN),
              fillOpacity = 0.7, 
              color = "#BDBDC3",
              weight = 1) %>%
  addLegend(pal = pal, 
            values = world_fertility$SP.DYN.TFRT.IN, 
            opacity = 0.7, 
            title = "Taux de fécondité")


### 2eme carte ###

library(leaflet)
library(sp)
library(rgdal)
library(rworldmap)

# Sélectionner les variables nécessaires
data <- select(fertility, iso3c, SP.DYN.TFRT.IN)

# Fusionner avec les données de la carte mondiale
map_data <- joinCountryData2Map(data, joinCode = "ISO3", nameJoinColumn = "iso3c")

# Créer la carte Leaflet
leaflet(map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Ajouter des tuiles de fond
  addPolygons(fillColor = ~colorQuantile("YlOrRd", map_data$SP.DYN.TFRT.IN)(SP.DYN.TFRT.IN),
              fillOpacity = 0.7, color = "#BDBDC3", weight = 1) %>%  # Ajouter les polygones des pays
  addLegend(position = "bottomright", pal = colorQuantile("YlOrRd", map_data$SP.DYN.TFRT.IN),
            values = map_data$SP.DYN.TFRT.IN, title = "Taux de fécondité")  # Ajouter la légende

# Sélectionner les variables d'intérêt :
fertility <- fertility[, c("iso3c", "fertility")]

# Convertir les noms des pays en minuscules pour un meilleur alignement avec les données cartographiques :
fertility$iso3c <- tolower(fertility$iso3c)

### 3eme carte ####

world <- map_data("world")

# Jointure des données avec les données de la carte
fertility_map <- left_join(world, fertility, by = c("region" = "Country"))

# Création de la carte leaflet
#output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lng = -95, lat = 40, zoom = 3) %>%
    addPolygons(
      data = fertility_map,
      fillColor = ~colorBin("YlOrRd", value = fertility, bins = 5),
      fillOpacity = 0.8,
      color = "#BDBDC3",
      weight = 1,
      group = "fertility",
      popup = paste0("<b>Country:</b> ", region, "<br>",
                     "<b>Fertility rate:</b> ", round(fertility, 2))
    ) %>%
    addLegend(
      position = "bottomright",
      title = "Fertility rate",
      colors = colorBin("YlOrRd", value = fertility, bins = 5),
      labels = c("< 2.0", "2.0 - 2.5", "2.5 - 3.0", "3.0 - 3.5", "> 3.5"),
      group = "fertility"
    )
#})

###### VISU TAUX DE FECONDITE #####
  taux_fecondite <- read.csv("BAILLEUL_DUCLOS/data/taux_fecondite.csv", header= TRUE, sep=',')
  taux_fecondite$TIME <- as.numeric(taux_fecondite$TIME)
  taux_fecondite$LOCATION <- as.factor(taux_fecondite$LOCATION)
  levels(taux_fecondite$LOCATION) <- c("Argentine", "Australie", "Autriche", "Belgique", "Bulgarie", "Brésil", "Canada", "Suisse", "Chili", "Chine", "Colombie", "Costa Rica", "Chypre", "République Tchèque", "Allemagne", "Danemark", "Espagne", "Estonie", "Union Européenne", "Finlande", "France", "Royaume-Uni", "Grèce", "Croatie", "Hongrie", "Indonésie", "Inde", "Irlande", "Islande", "Israël", "Italie",  "Japon", "Corée", "Lituanie", "Luxembourg", "Lettonie", "Mexique", "Malte", "Pays-Bas", "Norvège", "Nouvelle Zélande", "OAVG", "Pérou", "Pologne", "Portugal", "Roumanie", "Russie", "Arabie Saoudite", "Slovaquie", "Slovénie", "Suède", "Turquie", "États-Unis", "Afrique du Sud")       




