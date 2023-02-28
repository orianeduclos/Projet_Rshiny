

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # load custom stylesheet
  includeCSS("www/custom.css"),

  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

dashboardPage(skin='purple',
  dashboardHeader(title = "La natalité 👶", 
                  dropdownMenu(type="message", messageItem(from = "Margaux et Oriane", message="Bienvenue sur notre application 👶",icon=icon("envelope-open"), time = "Now"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("door-open")),
      menuItem(" Dans les pays du monde", tabName = "pays", icon = icon("earth"),
        menuSubItem("Présentation BDD", tabName = "bddpays"),
        menuSubItem("À l'échelle mondiale", tabName = "monde"),
        menuSubItem("Zoom sur les continents", tabName = "qqlpays")),
      menuItem(" En France", tabName = "france", icon = icon("location-dot"),
        menuSubItem("Présentation BDD", tabName = "bddfrance"), 
        menuSubItem("Traitement", tabName = "traitementfrance")),
      menuItem(" Dans une maternité", tabName = "mater", icon = icon("baby"),
        menuSubItem("Présentation BDD", tabName = "bddmater"), 
        menuSubItem("Régression", tabName = "regressionmater"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "accueil",
        h1("Bonjour")
        ),
      tabItem(
        tabName = "bddpays",
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
      ),
      
      tabItem(
        tabName = "monde",
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
            highchartOutput("graphique_pays")
          ), 
          tabPanel(
            title = "Graphique sur un pays", 
            sidebarPanel(
              selectInput(inputId = "pays_seul", label = "Choisissez un pays", choices = unique(taux_fecondite$LOCATION))
            ),
            plotOutput("graphique_pays_indiv")
          )
        )
      ),
      
      tabItem(
        tabName = "bddfrance", 
        
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
      ),
     
      
      tabItem(
        tabName = "traitementfrance",
        
        tabsetPanel(
          # Create a "Word cloud" tab
          tabPanel(
            title = "Word cloud",
        #### world cloud ####
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            selectInput("selection", "Choissisez une année:",
                        choices = c("année")),
            
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 50, value = 15),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  max = 300,  value = 100)
          ),
          
          # Show Word Cloud
          mainPanel(
            plotOutput("wordcloud")
          )
        )
          )
        )
      ), 

      tabItem(
        tabName = "bddmater", 
        
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
      ), 
      
      tabItem(
        tabName = "regressionmater", 
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
              )
            )
          ), 
          tabPanel(
            title = "Régression graphique", 
            plotOutput("rl")
          )
        )
      )
    )
  )
))
