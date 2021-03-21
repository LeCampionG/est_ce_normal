library(shinythemes)
library(shiny)
library(shinydashboard)
library(sparkline)
library(funModeling)
library(knitr)
library(stats)

options(shiny.maxRequestSize = 30*1024^2)


ui<-fluidPage(theme = shinytheme("slate"),
              navbarPage(tags$a("Est-ce bien normal ?",href = 'https://github.com/LeCampionG/est_ce_normal',
                                icon("github"), target="_blank"),
                         tabPanel("A propos",
                                  #code Html
                                  h2(strong("Bienvenue sur l'application Est-ce bien normal !!!")),
                                  br(),
                                  p(strong("Est-ce bien normal ?"),"vous permet de vérifier la normalité de vos données !"),
                                  p("Je suis d'accord avec vous, nous sommes tous différent et finalement qu'est ce que la normalité? N'est-elle pas subjective et finalement le besoin de norme est-il justifié ?"),
                                  p("En tout cas la statistique a une réponse très claire à ces questions, la normalité c'est important cela s'appelle aussi la loi de Gauss et ça ressemble à ça :"),
                                  imageOutput("myImage"),
                                  br(),
                                  br(),
                                  p("Il s'agit de la distribution", em(strong("reine")), "ou,", em(strong("idéale")), "en statistique. C'est sur cette distribution que se base les tests paramétriques (Corrélation de Pearson, Régression linéaire...)."),
                                  p("Ainsi, pour choisir le test que vous devrez réaliser que ce soit dans le cas où vous cherchez à déterminer l'existence de liens entre vos variables ou si vous cherchez à déterminé ou prédire l'influence de certaines variables sur d'autres, la première chose à faire et de vérifier si vos données suivent cette loi normale."),
                                  br(),
                                  p("Rassurez-vous! Vous ne jouez ni votre vie ni vos recherches ici. Si vos données on une distribution qui n'a rien de normal (ce sera très vraissemblablement le cas) il existe bien d'autres solutions! Avec notamment les tests non paramétriques (le plus connu étant le chi²). Par ailleurs je vous propose :"),
                                  p("- Pour les calculs de corrélations je vous invite pour un peu de théorie à vous rendre sur ce", strong(a("tutoriel", href="http://ouvrir.passages.cnrs.fr/wp-content/uploads/2019/04/rapp_correlation.html")),"et pour la pratique sur l'application", strong(a("Mon nom est Pearson", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Mon_nom_est_Pearson/")),". Vous pourrez y faire des corrélations avec Pearson si vos données suivent la loi normale mais vous pourrez aussi utiliser les test non paramétriques de calcul de corrélation (Spearman et Kendall)."),
                                  p("- Et pour l'étude de causalité ou les prédictions rendez-vous sur l'application", strong(a("BaobARD",href="https://analytics.huma-num.fr/Gregoire.LeCampion/Arbre_de_decision/")),", cette application vous permmettra de réaliser des arbres de décision ! La solution à tous vos problèmes! Pour plus d'information rendez-vous sur ce", strong("tutoriel"),"."),
                                  br(),
                                  p("Cette application vous propose donc de calculer et de vérifier la normalité de vos données à l'aide de 3 tests statistiques différents."),
                                  p("Vous pourrez dans un premier temps charger vos données. L'application n'accepte que les fichier csv au format utf-8 et il vous reviendra de préciser les bonnes options de configuration de votre base de données (séparateur...)"),
                                  p("Dans cet onglet d'import vous aurez également la possibilité de visualiser la distribution de vos variables sous  2 formes graphiques différentes : soit des diagrammes en barres ou des diagerammes de densité. Ces visualisations graphique pourront vous donner une première indications sur la distribution théorique de vos données. Mais ils ne vous suffiront pas à déterminer la normalité de vos variables."),
                                  br(),
                                  p("L'onglet Mesure de la normalité vous permettra finalement de déterminer si statistiquement la distribution de vos variables peut s'apparenter à la distribution de la loi normale."),
                                  p("Il faudra choisir la variable que vous désirez tester puis lire le résultat affiché. Vous aurez alors le choix entre 3 tests différents qui fonctionne tous sur le même principe malgré leurs subtilités."),
                                  p("- Le test de", strong("Shapiro-Wilk"), ", basé sur la statistique W, comparé aux autres tests il est très puissant pour les petits effectifs (n=<50), mais plus faible sur les grands échantillons. Il est considéré comme très fiable car il ne sert qu'à mesurer la normalité d'une distribution. Certain le considère comme le test de normalité de référence."),
                                  p("- Le", strong("Kolmogorov-Smirnov"), ", il s'agit du test le plus connus pour vérifier la normalité d'une distribution même s'il ne s’appliquent pas seulement à la vérification de l’adéquation à une loi normale. Il tient compte de la moyenne et de la variance des données."),
                                  p("- Le test", strong("d'Anderson-Darling"), ", est une bonne alternative au Kolmogorov-Smirnov. Ce test en est une variante, à la différence qu’il donne plus d’importance aux queues de distribution. De ce point de vue, ce test est plus indiqué dans la phase d’évaluation des données précédant la mise en œuvre d’un test paramétrique (comparaison de moyenne, de variances, etc….)"),
                                  br(),
                                  p("La lecture et l'analyse de ces tests constitue une exeption dans l'analyse des tests statistiques. En effet, pour une fois on cherchera ici à accepter l'hypothèse nulle et on pourra considérer que notre distribution suit une loi normale si le résultat de notre test est non significatif c'est à dire p-value>0.05 !"),
                                  br(),
                                  h3(strong("Citation")), 
                                  p("Dans l'éventualité où vous utiliseriez cette application dans le cadre d'une publication, vous pouvez éventuellement citer cet outil comme ceci :"),
                                  p(strong("Le Campion G. ", a("Est-ce bien normal: une application  pour vérifier la distribution des données et leur normalité.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Est_ce_normal/")," Pôle ARD UMR 5319 UMR Passages. 2019."))
                                  
                         ),
                         
                         tabPanel("Import données et distribution",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(fileInput("file1", "Charger un fichier CSV",
                                                           multiple = FALSE,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                                      ".csv")),
                                                 h5(helpText("Le poid des fichier est limité à 30Mb")),
                                                 tags$hr(),
                                                 h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                                 # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                                 checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                                 #déterminer le séparateur de champ
                                                 radioButtons("sep", "Séparateur de champ",
                                                              choices = c(Virgule = ",",
                                                                          "Point Virgule" = ";",
                                                                          Tab = "\t"),
                                                              selected = ","),
                                                 #Séparateur décimale
                                                 radioButtons("dec", "Symbole décimale",
                                                              choices = c(Virgule = ",",
                                                                          Point = "."),
                                                              selected = "."),
                                                 #déterminer séparateur de texte
                                                 radioButtons("quote", "Séparateur de texte",
                                                              choices = c("Aucun" = "",
                                                                          "Guillemet double" = '"',
                                                                          "Guillemet simple" = "'"),
                                                              selected = '"'),
                                                 #Choix du mode de visualistaion
                                                 radioButtons("disp", "Visualiser",
                                                              choices = c("Uniquement les 1eres lignes" = "head",
                                                                          "Ensemble des données"= "all"),
                                                              selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("1- Mes données",
                                               uiOutput("tb1")
                                      ),
                                      tabPanel("2- Distribution barplot",
                                               plotOutput("distribution", height="750px")
                                      ),
                                               
                                      tabPanel("2- Distribution courbe de densité",
                                            plotOutput("density", height="750px"))
                                    )
                                    ))),
                         tabPanel("Mesure de la normalité",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h3("Variables à analyser"),
                                      selectizeInput("selectVD", label="Choix de la variable", choices=NULL, multiple=FALSE)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Shapiro-Wilk",
                                                 h4("Test utilisé"),
                                                 verbatimTextOutput("sw_name_result"),
                                                 #h4("Variable analysée"),
                                                 #verbatimTextOutput("sw_var_result"),
                                                 h4("Coefficient"),
                                                 verbatimTextOutput("sw_w_result"),
                                                 h4("Significativité"),
                                                 verbatimTextOutput("sw_p_result"),
                                                 h5("Rappel : Dans le cas des tests de normalité nous cherchons ici à accepter l'hypothèse nulle. Nous pourrons donc considérer que notre variables suit une loi normale si p-value > 0.05")
                                        ),
                                        tabPanel("Kolmogorov-Smirnov ",
                                                 h4("Test utilisé"),
                                                 verbatimTextOutput("ks_name_result"),
                                                 #h4("Variable analysée"),
                                                 #verbatimTextOutput("ks_var_result"),
                                                 h4("Coefficient"),
                                                 verbatimTextOutput("ks_k_result"),
                                                 h4("Significativité"),
                                                 verbatimTextOutput("ks_p_result"),
                                                 h5("Rappel : Dans le cas des tests de normalité nous cherchons ici à accepter l'hypothèse nulle. Nous pourrons donc considérer que notre variables suit une loi normale si p-value > 0.05")
                                        ),
                                        tabPanel("Anderson-Darling",
                                                 h4("Test utilisé"),
                                                 verbatimTextOutput("ad_name_result"),
                                                 #h4("Variable analysée"),
                                                 #verbatimTextOutput("ad_var_result"),
                                                 h4("Coefficient"),
                                                 verbatimTextOutput("ad_a_result"),
                                                 h4("Significativité"),
                                                 verbatimTextOutput("ad_p_result"),
                                                 h5("Rappel : Dans le cas des tests de normalité nous cherchons ici à accepter l'hypothèse nulle. Nous pourrons donc considérer que notre variables suit une loi normale si p-value > 0.05")
                                        )
                                      )
                                    )
                                  )
                                  
                         )
              )
)



server <- function(input, output, session) {
  

  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(funModeling)
  library(viridis)
  library(RColorBrewer)
  library(reshape)
  library(nortest)
  
  #####################################################
  # 5. Charger les données importées et les visualiser#
  #####################################################
  
  output$myImage <- renderImage({
    list(
      src = "img/loi_normale.png",
      contentType = "image/png",
      alt = "Loi de Gauss"
    )
  }, deleteFile = FALSE)
  
  
  data <- reactive({
    file1 <- input$file1
    req(file1)
    data <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, dec =input$dec, quote = input$quote)
    data
  })
  
  output$table <- renderTable({
    req(input$file1)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
    df <- reactive({ read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              dec =input$dec,
                              quote = input$quote)
    })
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  output$distribution <- renderPlot({
    plot_num(dat())
  })
  
  
  plot_num_density <- function (data, path_out = NA) 
  {
    wide_data = suppressMessages(melt(data))
    p = ggplot(data = wide_data,  aes(x = value)) + 
      geom_density( na.rm = T) + 
      scale_fill_viridis_d()+
      facet_wrap(~variable, 
                 scales = "free") + aes(fill = variable) + guides(fill = FALSE)
    if (!is.na(path_out)) {
      export_plot(p, path_out, "density")
    }
    plot(p)
  }
  
  
  output$density<- renderPlot({
    plot_num_density(dat())
  })
  
  ################################################################
  # 6. Charger les données et maj des selectizeInput pour l'arbre#
  ################################################################ 
  dat <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, dec =input$dec, quote = input$quote)
    vars <- colnames(dataSet)
    #row <- nrow(dataSet)
    updateSelectizeInput(session, "selectVD", "Choix de la variable", choices = vars)
    dataSet
  })
  
  datred <- reactive({
    dat()[,c(as.numeric(input$selectVd))]
  })
  
  
  shapiro<- reactive({
    vd <- dat()[,input$selectVD]
    s1<- shapiro.test(vd)
  })
  
  kilmogorov<- reactive({
    vd <- dat()[,input$selectVD]
    s1<- ks.test(vd, "pnorm", mean(vd), sd(vd))
  })
  
  anderson<- reactive({
    vd <- dat()[,input$selectVD]
    s1<- ad.test(vd)
  })
  
  output$sw_name_result <-renderPrint({
    shapiro()$method})
 
  #output$sw_var_result <-renderPrint({
    #shapiro()$data.name})
  
  output$sw_w_result <-renderPrint({
    shapiro()$statistic})
  
  output$sw_p_result <-renderPrint({
    shapiro()$p.value})
  
  output$ks_name_result <-renderPrint({
    kilmogorov()$method})
  
  #output$ks_var_result <-renderPrint({
    #kilmogorov()$data.name})
  
  output$ks_k_result <-renderPrint({
    kilmogorov()$statistic})
  
  output$ks_p_result <-renderPrint({
    kilmogorov()$p.value})
  
  output$ad_name_result <-renderPrint({
    anderson()$method})
  
  #output$ad_var_result <-renderPrint({
    #anderson()$data.name})
  
  output$ad_a_result <-renderPrint({
    anderson()$statistic})
  
  output$ad_p_result <-renderPrint({
    anderson()$p.value})
  
  
}


shinyApp(ui=ui,server=server)


