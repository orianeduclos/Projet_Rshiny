

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
        menuSubItem("Visualisation", tabName = "visu_bddmater", icon = icon("eye")), 
        menuSubItem("Régression simple", tabName = "regsimple", icon = icon("chart-line")),
        menuSubItem("Régression multiple", tabName = "regressionmater", icon = icon("chart-line")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "accueil",
        tags$h1("Bonjour ! Bienvenue dans notre appplication de visualisation de la natalité"),
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
              dataTableOutput("summary_pays")
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
                #selectInput(inputId = "Year", label = "Années", choices = unique(world_fertility$year))
              ),
              br(),
              p(strong("Taux de fertilité des pays du monde")),
              br(),
              leafletOutput("map"),
              br(),
             textOutput("texte_carte")
            ), 
            tabPanel(
              title = "Graphique sur les pays", 
                highchartOutput("graphique_pays"), 
              fluidRow(
                p(strong("Interprétation : "),"nous remarquons une tendance globale à la baisse pour tous les pays au fur et à mesure des années. L'Arabie Saoudite se démarque des autres car elle a un taux de fécondité plus élevé.")
              )
            ), 
            tabPanel(
              title = "Graphique sur un pays", 
                selectInput(inputId = "pays_seul", label = "Choisissez un pays", choices = unique(taux_fecondite$LOCATION)),
              plotlyOutput("graphique_pays_indiv"), 
              br(),
              p("Nous remarquons que pour n'importe quel pays, le taux de fertilité a baissé entre 1960 et 2021. Cela nous permet de voir plus en détail chaque pays, l'onglet précédent ayant une échelle plus grande.")
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
              dataTableOutput("summary_france")
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
                  actionButton("update", "Change")
                  
                ),
                
                # Affichage du word cloud
                mainPanel(
                  plotOutput("wordcloud"), 
                  br(),
                  downloadButton(outputId="export",label= "Cliquez pour télécharger le graphique")
                )
              )
            ), 
            tabPanel(
              title = "Prénoms au fur et à mesure des années", 
              fluidRow(
                textInput("prenom_bebe", "Prénom du bébé", value = "LAURENT"), 
                p(strong("Indications")," : pour rechercher un prénom, écrivez le en majuscule et sans accent. ")
              ), 
              fluidRow(
                plotlyOutput("plot_bebe"), 
                br(),
                downloadButton(outputId="export_bebe",label= "Cliquez pour télécharger le graphique")
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
              dataTableOutput("summary_bebe")
            )
          )
        )
      ), 
      tabItem(
        tabName = "visu_bddmater", 
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Visualisation",
              tags$h6("L'objectif de cet onglet est de visualiser quelques statistiques descriptives"),
              fluidRow(
                column(
                  6, amChartsOutput(outputId = "amchart_boxplot")
                ),
                column(
                  6, amChartsOutput(outputId = "amchart_jauge")
                )
              ),
              br(),
              fluidRow(
                  amChartsOutput(outputId = "amchart_pie"), 
                  br()
              ),
              amChartsOutput(outputId = "amchart_bar")
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
              title = "Régression sortie", 
              sidebarLayout(
                sidebarPanel(
                  p("Sélectionner la variable à expliquer"),
                  selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("PoidsBB", "TailleBB")),
                  p("Sélectionner les variables explicatives"),
                  checkboxGroupInput(inputId = "IndVar", label = "Independent Variables", choices = list("PoidsBB", "TailleBB", "Nbsem", "PoidsPlacenta", "AgedelaMere", "NaissMere", "TailMere", "PoidsMere", "Agedupere", "NaisPere", "TailPere", "PoidsPere", "NbGrossess", "NbEnfants", "NbIVG", "NbFC", "DureeTrava", "IMCMere"), selected = "TailleBB")
                ),
                mainPanel(
                  verbatimTextOutput(outputId = "RegSum"),
                  verbatimTextOutput(outputId = "IndPrint"),
                  verbatimTextOutput(outputId = "DepPrint"), 
                  valueBoxOutput("r2",width=6),
                  valueBoxOutput("fisher",width=6),
                  plotOutput(outputId = "matricecorr")
                )
              )
        )
      ),
      tabItem(tabName = "regsimple", 
              tags$h6("L'objectif de cet onglet est de visualiser les relations possiblement existantes entre les différentes variables de notre modèle vis à vis de la variable PoidsBB. "),
              
              radioGroupButtons(
                inputId = "variable_simple",
                label = "Choisissez la variable à mettre en relation avec le poids du bébé",
                choices = c("TailleBB", "Nbsem", "PoidsPlacenta", "AgedelaMere", "NaissMere", "TailMere", "PoidsMere", "Agedupere", "NaisPere", "TailPere", "PoidsPere", "NbGrossess", "NbEnfants", "NbIVG", "NbFC", "DureeTrava", "IMCMere"),
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