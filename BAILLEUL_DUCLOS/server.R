

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
    paste("Le taux de fertilité dans le monde est un indicateur important de la santé démographique des populations. Il mesure le nombre moyen d'enfants qu'une femme peut espérer avoir tout au long de sa vie reproductive. En ",fertility_reactive()$year, "le taux de fertilité mondial était d'environ ",round(mean(fertility_reactive()$SP.DYN.TFRT.IN, na.rm=TRUE),2))
  })
  
  # Graphique représentant tous les pays 
  output$graphique_pays <- renderHighchart({
    hchart(
      taux_fecondite, "line", 
      hcaes(x = TIME, y =  Value, group = LOCATION)
    )
  })
  
  # Graphique dans lequel on peut choisir le pays 
  output$graphique_pays_indiv <- renderPlot({
    df <- taux_fecondite |>  dplyr::filter(LOCATION==input$pays_seul)
    ggplot(df)+aes(x=TIME,y=Value)+
      geom_point()+geom_smooth()+theme_bw()
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
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
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
      geom_smooth(method="lm")
  })

  
}
