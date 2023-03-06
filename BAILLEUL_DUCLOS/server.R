

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
  
  prenom_data <- reactive({
    data[data$annais == input$year_prenom, ]
  })
#### Partie Acceuil ####
  
  ## BANNIERE CHIFFRE CLES
  
  output$Age_moyen_maman_France=renderValueBox({
    valueBox(
      paste0(round((mean(bebe$AgedelaMere)),1)," ans"),"Age moyen de la mère à la naissance de l'enfant", 
      color = "red")
  })
  
  output$Taux_fertilites=renderValueBox({
    
    valueBox(
      paste0("+",round(world_fertility$SP.DYN.TFRT.IN,2)), "de naissance par femme dans le monde", 
      color = "red")
  })
  
  output$Nombre_semaine=renderValueBox({
    
    valueBox(
      paste0(round(mean(bebe$Nbsem),2)," Semaine"), "Moyenne du Nombre de semaine du temps de gestation chez les femmes ", 
      color = "red")
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
                title = "Taux de fécondité") |> 
      # Ajout d'un rectangle à la main sur la france 
      addRectangles(
        lng1 = -5.11, lat1 = 52.10,
        lng2 = 10.92, lat2 = 40.25,
        color = "green",
        popup = "France",
        fill = FALSE)
  })
  
  # Texte en dessous carte 
  output$texte_carte <- renderText({
    paste("Le taux de fertilité dans le monde est un indicateur important de la santé démographique des populations. Il mesure le nombre moyen d'enfants qu'une femme peut espérer avoir tout au long de sa vie reproductive. En ", input$Year, "le taux de fertilité mondial était d'environ ",round(mean(fertility_reactive()$SP.DYN.TFRT.IN, na.rm=TRUE),2))  
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
  output$graphique_pays_indiv <- renderPlot({
    df <- taux_fecondite |>  dplyr::filter(LOCATION==input$pays_seul)
    ggplot(df)+aes(x=TIME,y=Value)+
      geom_point()+geom_smooth()+theme_bw()
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
  
  # Générer le word cloud
  output$wordcloud <- renderPlot({
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
  
  
  output$export <- downloadHandler(
    filename = function() {
      paste("Worldcloud-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      wordcloud(words = d$word, freq = d$freq, min.freq = input$freq,
                max.words=input$max, colors = brewer.pal(8,"Dark2"), random.order=FALSE, rot.per=0.35)
      dev.off()
    }
  )
  
  # Graphique sur les bébés 
  output$plot_bebe <- renderPlotly({
    df <- prenom_annees |>  dplyr::filter(preusuel==input$prenom_bebe)
    p <- ggplot(df)+aes(x=annais,y=nombre)+
      geom_point() 
    ggplotly(p)
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
  
  
  lm1 <- reactive({
    lm(reformulate(input$IndVar, input$DepVar), data = bebe)
  })
  
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  
  output$rl <- renderPlot({
    ggplot(bebe) + 
      aes(x = PoidsBB, y = TailleBB) + 
      geom_point()+
      geom_smooth(method="loess")
  })


}
