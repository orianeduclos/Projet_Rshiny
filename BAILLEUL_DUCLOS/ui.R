

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # load custom stylesheet
  includeCSS("www/custom.css"),

  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

dashboardPage(
  dashboardHeader(title = "La natalité 👶", 
                  tags$li(class="dropdown",tags$a(href="https://www.worldometers.info/fr/", icon("earth"), " Worldometer", target = "_blank")),
                  dropdownMenu(type="message", messageItem(from = "Margaux et Oriane", message="Bienvenue sur notre application 👶",icon=icon("envelope-open"), time = "Now"))),
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        tags$br(),
        tags$img(style = 'display: block; margin-left:auto; margin-right: auto;', src='logo_natalite.png', width ='186'),
        tags$br()
      )),
      menuItem("Accueil", tabName = "accueil", icon = icon("door-open")),
      menuItem(" Dans les pays du monde", tabName = "pays", icon = icon("earth"),
        menuSubItem("Présentation BDD", tabName = "bddpays", icon = icon("database")),
        menuSubItem("À l'échelle mondiale", tabName = "monde", icon = icon("globe"))),
      menuItem(" En France", tabName = "france", icon = icon("location-dot"),
        menuSubItem("Présentation BDD", tabName = "bddfrance", icon = icon("database")), 
        menuSubItem("Traitement", tabName = "traitementfrance", icon = icon("flag"))),
      menuItem(" Dans une maternité", tabName = "mater", icon = icon("baby"),
        menuSubItem("Présentation BDD", tabName = "bddmater", icon = icon("database")), 
        menuSubItem("Régression", tabName = "regressionmater", icon = icon("chart-line")),
        menuSubItem("Régression simple", tabName = "regsimple", icon = icon("chart-line")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "accueil",
        tags$h1("Bonjour bienvenue dans notre appplication de visualisation de la natalité"),
        fluidRow(
          tags$h6("Les données sur les naissances permettent de comprendre les comportements de reproduction des populations et de planifier les services de santé en conséquence. Elles sont importantes pour les études démographiques, telles que la projection de la croissance de la population et la compréhension de la répartition géographique des naissances. De plus, ces données sont utilisées dans la recherche en santé publique pour mieux comprendre les facteurs qui influencent la santé maternelle et infantile, et pour développer de nouvelles interventions pour améliorer la santé des mères et des enfants."),
          valueBoxOutput("Taux_fertilite",width=4),
          valueBoxOutput("Age_moyen_maman_France",width=4),
          valueBoxOutput("Nombre_semaine",width=4)),
        # Ajouter une image GIF
        tags$img(src = "https://media.giphy.com/media/26xBJyMTcwM7rUyPe/giphy.gif"),
       
        
        ),
      tabItem(
        tabName = "bddpays",
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Présentation de la base de données", 
              dataTableOutput("visu_pays")
            ), 
            tabPanel(
              title = "Summary", 
              verbatimTextOutput("summary_pays")
            ),
          )
        )
      ),
      
      tabItem(
        tabName = "monde",
        tags$h6("L'objectif de cet onglet est de visualiser le taux de fécondité et le taux de fertilité dans le monde "),
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Carte", 
              sidebarPanel(
                selectInput(inputId = "Year", label = "year", choices = unique(world_fertility$year))
              ),
              leafletOutput("map"),
              br(),
             textOutput("texte_carte")
            ), 
            tabPanel(
              title = "Graphique sur les pays", 
              box(highchartOutput("graphique_pays")), 
              box(textOutput("texte_plsrs_pays"))
            ), 
            tabPanel(
              title = "Graphique sur un pays", 
              sidebarPanel(
                selectInput(inputId = "pays_seul", label = "Choisissez un pays", choices = unique(taux_fecondite$LOCATION))
              ),
              box(plotlyOutput("graphique_pays_indiv")), 
              box(textOutput("texte_pays_seul"))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "bddfrance", 
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Présentation de la base de données", 
              dataTableOutput("visu_france")
            ), 
            tabPanel(
              title = "Summary", 
              verbatimTextOutput("summary_france")
            )
          )
        )
      ),
     
      tabItem(
        tabName = "traitementfrance",
        tags$h6("L'objectif de cet onglet est de visualiser les prénoms en france entre 1900 et 2021"),
        fluidPage(
          tabsetPanel(id = "viz",
            # Create a "Word cloud" tab
            tabPanel(
              title = "Word cloud",
              #### world cloud ####
              # Titre de la page
              titlePanel("World cloud des prénoms en France"),
              # Zone de sélection des années
              sidebarLayout(
                sidebarPanel(
                  selectInput("year", "Sélectionnez une année :", choices = unique(prenom$annais)),
                  hr(),
                  sliderInput("freq",
                              "Fréquence Minumun:",
                              min = 1,  max = 100, value = 10),
                  sliderInput("max",
                              "Nombre Maximal de mots:",
                              min = 1,  max = 500,  value = 100),
                  hr(),
                  ##### À REVOIR #####
                  actionButton("update", "Change"),
                  downloadButton(outputId="export",label= "Cliquez pour sauvegarder le graphique")
                  
                ),
                
                # Affichage du word cloud
                mainPanel(
                  plotOutput("wordcloud")
                )
              )
            ), 
            tabPanel(
              title = "Prénoms au fur et à mesure des années", 
              fluidRow(
                textInput("prenom_bebe", "Prénom du bébé", value = "LAURENT"), 
                box(p("Indications : pour rechercher un prénom, écrivez le en majuscule et sans accent. "))
              ), 
              fluidRow(
                plotlyOutput("plot_bebe")
              )
                )
              )
            )
          ), 

      tabItem(
        tabName = "bddmater", 
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Présentation de la base de données", 
              dataTableOutput("visu_bebe")
              
            ), 
            tabPanel(
              title = "Summary", 
              verbatimTextOutput("summary_bebe")
            ),
            tabPanel(
              title = "Visualisation",
              tags$h6("L'objectif de cet onglet est de visualiser quelques statistiques descriptives"),
              fluidRow(
                box(amChartsOutput(outputId = "amchart_boxplot")),
                box(textOutput("texte_boxplot_age"))
              ),
              fluidRow(
                box(amChartsOutput(outputId = "amchart_jauge")),
                box(amChartsOutput(outputId = "amchart_pie")),
              ),
                box(amChartsOutput(outputId = "amchart_bar"))
            ), 
            tabPanel(
              title = "Profil de la maman",
              tags$h6("L'objectif de cet onglet est de visualiser le poids du BB en fonction de la tranche de la maman"),
              sidebarLayout(
                sidebarPanel(
                  h4("Sélectionnez les variables pour afficher les résultats:"),
                  selectInput("age", "Âge de la maman :", choices = c("Tous" = "all", "Moins de 20 ans" = "lt20", "20-24 ans" = "20to24", "25-29 ans" = "25to29", "30-34 ans" = "30to34", "35-39 ans" = "35to39", "40 ans et plus" = "ge40"), selected = "all"),
                ),
                mainPanel(
                  colourInput(inputId = "color", label = "Couleur :", value = '#FAE5D3'),
                  plotlyOutput("graph")
                )
              )
              
            )
          )
        )
      ), 
      
      tabItem(
        tabName = "regressionmater", 
        tags$h6("L'objectif de cet onglet est de visualiser les relations existantes entre les différentes variables de notre modèle. Vous trouverez une regression simple et une regression multiple"),
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Régression sortie", 
              sidebarLayout(
                sidebarPanel(
                  p("Select the inputs for the Dependent Variable"),
                  selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("PoidsBB", "TailleBB")),
                  p("Select the inputs for the Independent Variable"),
                  checkboxGroupInput(inputId = "IndVar", label = "Independent Variables", choices = colnames(bebe), selected = "TailleBB")
                ),
                mainPanel(
                  verbatimTextOutput(outputId = "RegSum"),
                  verbatimTextOutput(outputId = "IndPrint"),
                  verbatimTextOutput(outputId = "DepPrint"), 
                  valueBoxOutput("r2",width=6),
                  valueBoxOutput("fisher",width=6)
                )
              )
            ), 
            tabPanel(
              title = "Régression graphique",
              
              box(
      title = "Analyse des corrélations des régresseurs du modèle", solidHeader=T,
                       width = 600,height=700, collapsible = T,
                       plotOutput("correlation")),
            )
          )
        )
      ),
      tabItem(tabName = "regsimple", 
              tags$h6("L'objectif de cet onglet est de visualiser les relations existantes entre les différentes variables de notre modèle vis à vis de la variable 'Nombre de semaine de gestation."),
              
              radioGroupButtons(
                inputId = "variable_simple",
                label = "Choisissez la variable à mettre en relation avec le nombre de semaine de gestation",
                choices = c( "TailleMere","TaillePere", "PoidsMere","NbGrossess","NbEnfants"),
                individual = TRUE,
                checkIcon = list(yes = tags$i(class = "fa fa-circle",style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o",style = "color: steelblue"))),
              
              radioGroupButtons(
                inputId = "choix_graphe",
                label = "Choisissez le type de visualisations (univariées ou bivariées):", 
                choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                justified = TRUE),
              
          
              
              box(
                title = "", solidHeader=T,
                width = 12,height=700, collapsible = T,
                plotlyOutput("nuage_point"),tags$p(""))
      )
    )
  )
)
)