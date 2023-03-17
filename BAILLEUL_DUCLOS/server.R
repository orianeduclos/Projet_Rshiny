

##### Partie Server #####

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactivité de la carte en fonction des années 
  fertility_reactive <- reactive({
    subset(
      world_fertility, 
      year == input$Year
    )
  })
  

#### Partie Acceuil ####
  
  ## BANNIERE CHIFFRE CLES
  
  output$Age_moyen_maman_France <- renderValueBox({
    valueBox(
      paste0(round((mean(bebe$AgedelaMere)),1)," ans"),"Âge moyen de la mère à la naissance de l'enfant", 
      icon = icon("person-breastfeeding"), color = "yellow")
  })
  
  output$Taux_fertilite <- renderValueBox({
    valueBox(
      paste0(round(mean(world_fertility$SP.DYN.TFRT.IN, na.rm = TRUE), 2), " enfants"), "par femme en moyenne dans le monde", 
      icon = icon("earth"), color = "yellow")
  })
  
  output$Nombre_semaine <- renderValueBox({
    
    valueBox(
      paste0(round(mean(bebe$Nbsem),2)," semaines"), "de temps de gestation moyen chez les femmes", 
      icon = icon("baby"), color = "yellow")
  })
  
  
 
#### Partie Pays ####    

  output$visu_pays <- DT::renderDataTable({
    datatable(taux_fecondite, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  output$summary_pays <- renderPrint({
    summary(taux_fecondite)
    
  })
  
  
  
 
  # Création de la carte leaflet
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      addPolygons(data = fertility_reactive(), 
                  label = ~ fertility_reactive()$name_sort,
                  opacity= 1,
                  dashArray = "2",
                  fillColor = ~pal(SP.DYN.TFRT.IN),
                  fillOpacity = 0.8, 
                  color = "#BDBDC3",
                  highlightOptions = highlightOptions(color = "#666", weight = 2, dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                  weight = 1,
                  popup = paste0("<b>Country:</b> ",fertility_reactive()$name_sort, "<br>",
                                 "<b>Fertility rate:</b> ", round(fertility_reactive()$SP.DYN.TFRT.IN, 2))
      ) |> 
      addLegend(pal = pal, 
                values = fertility_reactive()$SP.DYN.TFRT.IN, 
                opacity = 0.7, 
                title = "Taux de fécondité")
     
  })
  
  # Texte en dessous carte 
  output$texte_carte <- renderText({
    paste("Le taux de fertilité dans le monde est un indicateur important de la santé démographique des populations. Il mesure le nombre moyen d'enfants qu'une femme peut espérer avoir tout au long de sa vie reproductive. En ", input$Year, "le taux de fertilité mondial était d'environ ",round(mean(fertility_reactive()$SP.DYN.TFRT.IN, na.rm=TRUE),2),"Ce taux varie considérablement d'un pays à l'autre et même au sein des pays. Les pays les plus pauvres et les moins développés ont généralement des taux de fertilité plus élevés, tandis que les pays les plus riches et les plus développés ont tendance à avoir des taux plus bas. Les facteurs qui influencent le taux de fertilité sont nombreux et complexes. L'âge est l'un des facteurs les plus importants, car les femmes ont une période limitée de fécondité, qui diminue avec l'âge. Les femmes qui ont leur premier enfant à un âge plus avancé ont tendance à avoir moins d'enfants au total. L'éducation des femmes est également un facteur important, car les femmes qui ont accès à l'éducation ont tendance à avoir des taux de fertilité plus bas, en partie parce qu'elles ont des opportunités professionnelles et économiques plus nombreuses qui peuvent les empêcher d'avoir de nombreux enfants. Les normes culturelles et religieuses sont également importantes. Dans certaines cultures, avoir de nombreux enfants est considéré comme un signe de réussite ou de statut, tandis que dans d'autres cultures, les femmes sont encouragées à limiter leur nombre d'enfants. Les politiques gouvernementales, telles que les programmes de planning familial et l'accès à des méthodes contraceptives, peuvent également influencer le taux de fertilité. Un taux de fertilité élevé peut avoir des avantages, tels que le maintien de la population, mais il peut également présenter des défis, tels que la pression sur les ressources, les problèmes de santé maternelle et infantile et la pauvreté. À l'inverse, un taux de fertilité faible peut également avoir des avantages, tels que la réduction de la pression sur les ressources, mais il peut également présenter des défis, tels que le vieillissement de la population et les problèmes économiques associés à une population en déclin.")  
  })
  
  # Graphique représentant tous les pays 
  output$graphique_pays <- renderHighchart({
    hchart(
      taux_fecondite, "line", 
      hcaes(x = TIME, y =  Value, group = LOCATION)
    )
  })
  
  # Texte pour les pays 
  output$texte_plsrs_pays <- renderText({
    paste("Nous remarquons que blabla")
  })
  
  # Graphique dans lequel on peut choisir le pays 
  output$graphique_pays_indiv <- renderPlotly({
    df <- taux_fecondite |>  dplyr::filter(LOCATION==input$pays_seul)
    ggplot(df)+aes(x=TIME,y=Value)+
      geom_line(size = 1)+theme_bw() + 
      labs(title = paste("Évolution du taux de fécondité du pays ", input$pays_seul),
           x = "Années",
           y = "Taux de fécondité")
  })
  
  # Texte pour les pays 
  output$texte_pays_seul <- renderText({
    paste("Nous remarquons que blabla", input$pays_seul)
  })
  
#### Partie France #### 

  output$visu_france <- DT::renderDataTable({
    datatable(prenom, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  output$summary_france <- renderPrint({
    summary(prenom)
  })
  
  # Sélectionner les données en fonction de l'année choisie
  prenom_data <- reactive({
    prenom[prenom$annais == input$year, ]
  })
  
  # Générer le word cloud
  output$wordcloud <- renderPlot({
    input$update
    isolate({
      ## Créer un corpus des prénoms sélectionnés
      docs <- Corpus(VectorSource(prenom_data()$preusuel))
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
      
      ## Créer une matrice de termes-fréquences
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
      
      # Générer le word cloud
      wordcloud(words = d$word, freq = d$freq, min.freq = input$freq,
                max.words=input$max, colors = brewer.pal(8,"Set2"), random.order=FALSE, rot.per=0)
    })
  })
  

  
  output$export <- downloadHandler(
    filename = function() {
      paste("Worldcloud-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      ## Créer un corpus des prénoms sélectionnés
      docs <- Corpus(VectorSource(prenom_data()$preusuel))
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
      
      ## Créer une matrice de termes-fréquences
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
      
      # Générer le word cloud
      wordcloud(words = d$word, freq = d$freq, min.freq = input$freq,
                max.words=input$max, colors = brewer.pal(8,"Set2"), random.order=FALSE, rot.per=0)
      dev.off()
    }
  )
  
  # Graphique sur les bébés
  output$plot_bebe <- renderPlotly({
    df <- prenom_annees |>  dplyr::filter(preusuel==input$prenom_bebe)
    ggplot(df)+
      aes(x = annais, y = nombre) + 
      geom_line(size = 1) + 
      scale_color_hue(direction = 1) +
      labs(title = paste("Évolution du prénom", input$prenom_bebe),
           x = "Année de naissance",
           y = "Nombre de prénoms donnés")
  })
  
  # Exportation du graphique bébé 
  
  output$export_bebe <- downloadHandler(
    filename = function() {
      paste("Prénom", input$prenom_bebe, ".png", sep="")
    },
    content = function(file) {
      png(file)
      df <- prenom_annees |>  dplyr::filter(preusuel==input$prenom_bebe)
      ggplot(df)+
        aes(x = annais, y = nombre) + 
        geom_line(size = 1) + 
        scale_color_hue(direction = 1) +
        labs(title = paste("Évolution du prénom", input$prenom_bebe),
             x = "Année de naissance",
             y = "Nombre de prénoms donnés")
      dev.off()
    })
  
  
#### Partie Maternité ####  
 
   output$visu_bebe <- DT::renderDataTable({
    datatable(bebe, options = 
                list(scrollX = TRUE, 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
  })
  
  
  output$summary_bebe <- renderPrint({
    summary(bebe)
  })
  
  
  ### Onglet visualsiation  
  output$amchart_boxplot <- renderAmCharts({
    amBoxplot(AGEPARENT ~ Sexe_parent , col = "pink", data =bebe_sexe, ylab="Age du parent", main= "Boxplot de l'age du parent en fonction du sexe")
  })
  
  
  output$texte_boxplot_age <- renderText({
    paste("Nous remarquons que blabla")
  })
  
  output$amchart_jauge <- renderAmCharts({
    amAngularGauge(x = round(mean(bebe$Nbsem)), main= "Nombre de semaine de gestation moyenne") |> 
      amOptions(export = TRUE, exportFormat = "JPG")
  })
  
  output$amchart_bar <- renderAmCharts({
    Mode_travail <- as.data.frame(table(bebe$ModeTravai))
    colnames(Mode_travail) <- c("label","value")
    amBarplot(x = "label", y = "value", data = Mode_travail, horiz = TRUE) |> 
      amOptions(export = TRUE, exportFormat = "JPG")
  })
  
  output$amchart_pie <- renderAmCharts({
    Mode_accouchement <- as.data.frame(table(bebe$ModeAccouc))
    colnames(Mode_accouchement) <- c("label","value")
    amPie(data=Mode_accouchement, main="Mode d'accouchement")|> 
      amOptions(export = TRUE, exportFormat = "JPG")
  })
  
  ### Onglet profil maman###
  
  
  
  # Fonction pour filtrer les données en fonction des sélections de l'utilisateur
  filter_data <- reactive({
    bebe$age_class <- cut(bebe$AgedelaMere, breaks = c(0, 20, 25, 30, 35, 39, Inf), labels = c("lt20", "20to24", "25to29", "30to34", "35to39", "ge40"))
    if (input$age == "all") {
      return(bebe)
    } else {
      filtered_data <- bebe
      if (input$age != "all") {
        filtered_data <- filtered_data[filtered_data$age_class == input$age,]
      }
      
      
      return(filtered_data)
    }
  })
  
  # Fonction pour créer le graphique en fonction des données filtrées
  output$graph <- renderPlotly({
    data <- filter_data()
    p <- ggplot(data, aes(x = PoidsBB)) + 
      geom_density(fill = input$color, alpha = 0.3) +
      xlab("Poids du bébé à la naissance (en onces)") + 
      ylab("Densité") + 
      ggtitle("Profil moyen de la maman à l'accouchement")
    ggplotly(p)
  })
  
  ### Onglet regression ###
  
  lm1 <- reactive({
    lm(reformulate(input$IndVar, input$DepVar), data = bebepropre)
  })
  
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  
  
  ## MATRICE DE CORRELATION DES VARIABLES DU MODELE
  
  output$matricecorr <- renderPlot({
    var <- input$IndVar
    df <- bebepropre[var]
    corrplot(cor(df))
  })
  
  output$r2=renderValueBox({
    r2=summary(lm1())$r.squared
    
    if (r2<0.4){
      color="red"
        icon=icon("exclamation-triangle")
    }else if(r2>0.4&r2<0.6){
      color="orange"
        icon=NULL
    }else{
      color="green"
        icon=icon("check")
    }
    valueBox(
      round(r2,2),"R2 du modèle", 
      color = color,icon=icon)
  })
  
  output$fisher=renderValueBox({
    stat=summary(lm1())$fstatistic[[1]]
    valueBox(
      round(stat,2), "Statistique de fisher du modèle", 
      color = "purple")
  })
  

  
  ## VISUALISATION NUAGE DE POINT POUR LE MODELE SIMPLE
  output$nuage_point <- renderPlotly({
    
    variable_modele<- reactive({
      switch(input$variable_simple,
             
             "TailleMere" = bebe$TailMere,
             "TaillePere" = bebe$TailPere, 
             "PoidsMere"= bebe$PoidsMere,
             "NbGrossess" =bebe$NbGrossess,
             "NbEnfants" =bebe$NbEnfants,
             "TailleBB" = bebe$TailleBB,
             "Nbsem" = bebe$Nbsem
     
             )
    })
    
    type_graph<- reactive({
      switch(input$choix_graphe,
             "bar"=histo,
             "line"=nuage
            )
    })
    
    
    nuage=ggplot(bebe) +
      aes(x = PoidsBB, y = variable_modele()) +
      geom_point(shape = "circle", size = 1.5, colour = "#F08080") +
      labs(y = input$variable_simple,x = "PoidsBB",title = paste("Nuage de point de",input$variable_simple,"avec TailleBB")) +
      geom_smooth(method="lm")+
      
      theme_minimal()
    
    histo=ggplot(bebe) +
      aes(x = variable_modele()) +
      geom_histogram(bins = 30L, fill ="#F08080", colour = "#F08080") +
      labs(x = input$variable_simple,y = "Valeur",title = paste("Histogramme de la variable",input$variable_simple))+
      theme_minimal()
    
    type_graph()
    
  })
  

}
