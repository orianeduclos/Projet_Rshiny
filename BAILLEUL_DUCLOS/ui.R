

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
        tags$h1("Bonjour"), 
        tags$h2("Chiffres clés"),
        
        fluidRow(
          valueBoxOutput("Taux_fertilite",width=4),
          valueBoxOutput("Age_moyen_maman_France",width=4),
          valueBoxOutput("Nombre_semaine",width=4)),
        img(src = "bienvenue.jpg", height = "300", width = "300")
      
        
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
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Carte", 
              sidebarPanel(
                selectInput(inputId = "Year", label = "year", choices = unique(world_fertility$year))
              ),
              leafletOutput("map"),
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
              sidebarLayout(
                sidebarPanel(
                  textInput("prenom_bebe", "Prénom du bébé", value = "LAURENT")
                ),
                # Graphique des bébés 
                mainPanel(
                  plotlyOutput("plot_bebe"), 
                  box(p("Indications : pour rechercher un prénom, écrivez le en majuscule et sans accent. "))
                )
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
            )
          )
        )
      ), 
      
      tabItem(
        tabName = "regressionmater", 
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
                  verbatimTextOutput(outputId = "DepPrint")
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
              fluidRow(
                p("L'objectif de cet onglet est de visualiser les relations existantes entre les différentes variables de notre modèle vis à vis de la variable 'Nombre de semaine de gestation.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
              ),
              radioGroupButtons(
                inputId = "variable_simple",
                label = "Choisissez la variable à mettre en relation avec le nombre de semaine de gestation",
                choices = c("Nbsem"),
                individual = TRUE,
                checkIcon = list(yes = tags$i(class = "fa fa-circle",style = "color: steelblue"),no = tags$i(class = "fa fa-circle-o",style = "color: steelblue"))),
              
              radioGroupButtons(
                inputId = "choix_graphe",
                label = "Choisissez le type de visualisations (univariées ou bivariées):", 
                choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                justified = TRUE),
              
              colourInput(inputId = "color2", label = "Couleur :", value = '#579E7D'),
              
              box(
                title = "", solidHeader=T,
                width = 12,height=700, collapsible = T,
                plotlyOutput("nuage_point"),tags$p(""))
      )
    )
  )
)
)
