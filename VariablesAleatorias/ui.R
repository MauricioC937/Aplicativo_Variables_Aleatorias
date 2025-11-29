library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel(
    fluidRow(
      column(width=2,tags$img(src="epn.png", width="80px", height="80px")),
      column(width=10,h1('Números Aleatorios-Variables Aleatorias',
                         style="text-align:center;color:#132b60;
                         padding:18px;font-size:1.1em;font-family:roman"),
             h1('Dayanareth Estevez, Dennis Bazurto, Mauricio Caisaguano', 
                style="text-align:center;color:#132b60;
                         padding:18px;font-size:0.5em;font-family:roman"))
    )
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
                 numericInput("num",
                              "Numero de variables:",
                              min = 1,
                              max = 500,
                              value = 20),
                 radioButtons('metodo',
                              'Seleccione el método de generación;',
                              c('Congruencial multiplicativo',
                                'Congruencial mixto',
                                'Cuadrados Medios',
                                'Lehmer')),
                 hr(),
                 h3('Parámetros'),
                 numericInput('x0','Semilla inicial',min=99,max=2^31-1,value=151),
                 numericInput('a','Constante a:',min=1,max=2^31-1,value=31),
                 numericInput('m','Constante m:',min=1,max=2^31-1,value=73),
                 numericInput('c','Constante c:',min=1,max=2^31-1,value=97),
                 hr(),
                 actionButton('simular','Simular',icon = icon('circle-play'))
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      navbarPage(icon('book'),
                 tabPanel("Numéros Aleatorios",
                          verbatimTextOutput('code'),
                          plotOutput('grafico'),
                          verbatimTextOutput('resumen'),
                          hr(),
                          conditionalPanel(condition = "input.simular > 0",
                                           h3('Calculo de integrales'),
                                           fluidRow(
                                                  column(4, textInput("func", "Ingrese la función que desea evaluar")),
                                                  column(4,
                                                         selectInput("lim_inf_tipo", "Tipo de límite inferior:",
                                                                     choices = c("Finito" = "fin", "-∞" = "inf_menor"),
                                                                     selected = "fin")),
                                                  column(4,
                                                         selectInput("lim_sup_tipo", "Tipo de límite superior:",
                                                                     choices = c("Finito" = "fin", "+∞" = "inf_mayor"),
                                                                     selected = "fin"))
                                                ),
                                                
                                                # Mostrar los campos solo si son finitos
                                                conditionalPanel(condition = "input.lim_inf_tipo == 'fin'",
                                                                 numericInput("linf", "Límite inferior", value = 0)),
                                                conditionalPanel(condition = "input.lim_sup_tipo == 'fin'",
                                                                 numericInput("lsup", "Límite superior", value = 1)),
                                                
                                           hr(),
                                           plotOutput('graficofun'),
                                           h3('El resultado de la integral es:'),
                                           verbatimTextOutput('integral')
                          )
                 ),
                 tabPanel("Variables Aleatorias Discretas",
                                                 h3("Simulación de Variables Aleatorias Discretas"),
                                                 numericInput("num_disc", 
                                                 "Número de variables aleatorias a generar:", 
                                                 min = 1, max = 2000, value = 20),
                                                 hr(),
                                                 selectInput("MetodoDisc", "Seleccione la distribución:",
                                                 choices = c("Transformada Inversa",
                                                        "Geométrica",
                                                        "Poisson",
                                                        "Binomial",
                                                        "Binomial Negativa")),
                                                 hr(),
                                                 #Transformada inversa
                                                 conditionalPanel(
                                                 condition = "input.MetodoDisc == 'Transformada Inversa'",
                                                 helpText("Ingrese probabilidades separadas por coma: 0.2,0.5,0.3"),
                                                 textInput("probs", "Probabilidades:", "0.2,0.5,0.3"),            
                                                 helpText("Ingrese los valores asociados: 1,2,3"),
                                                 textInput("vals", "Valores:", "1,2,3")
                                                  ),
                                                 
                                                 #Geometrica
                                                 conditionalPanel(
                                                 condition = "input.MetodoDisc == 'Geométrica'",
                                                 numericInput("p_geom", 
                                                              "Probabilidad de éxito (p):", 
                                                              value = 0.3, min = 0.0001, max = 1)
                                                 ),
                                                
                                                #Poisson
                                                 conditionalPanel(condition = "input.MetodoDisc == 'Poisson'",
                                                 numericInput("lambda_pois", 
                                                              "Tasa λ:", 
                                                              value = 3, min = 0.0001)
                                                ),
                               
                                                #Binomial
                                                conditionalPanel(condition = "input.MetodoDisc == 'Binomial'",
                                                 numericInput("n_bin", "Número de ensayos (n):", 
                                                              min = 1, value = 10),
                                                 numericInput("p_bin", "Probabilidad de éxito (p):", 
                                                              min = 0, max = 1, value = 0.5)
                                               ),

                                              #Binomial Negativa
                                              conditionalPanel(condition = "input.MetodoDisc == 'Binomial Negativa'",
                                               numericInput("r_bin", "Número de éxitos deseados (r):", 
                                                        min = 1, value = 10),
                                               numericInput("p_bneg", "Probabilidad de éxito (p):", 
                                                        min = 0, max = 1, value = 0.5)
                                              ),
                                
                                              hr(),
                                
                                               actionButton("sim_disc", "Simular", icon = icon("dice")),
                        
                                               hr(),
                                               h4("Resultados de la Simulación"),
                                
                                verbatimTextOutput("disc_code"),
                                plotOutput("disc_grafico"),
                                verbatimTextOutput("disc_resumen")
                         ),
                tabPanel("Variables Aleatorias Continuas",
                          h3("Simulación de Variables Aleatorias Continuas"),
                          
                          selectInput("tipo", "Tipo de distribución:",
                                      choices = c("Exponencial", 
                                                  "F fija", 
                                                  "F por trozos",
                                                  "Método de Aceptación-Rechazo")),
                          
                          numericInput("n", "Número de valores a generar:", min = 1,max=10000,value=10),
                          
                          # Exponencial
                          conditionalPanel(
                            "input.tipo == 'Exponencial'",
                            numericInput("lambda", "λ:", 5, min = 0.00001)
                          ),
                          
                          # F fija
                          conditionalPanel(
                            "input.tipo == 'F fija'",
                            helpText("Ingrese F^{-1}(u). Puede escribir la función completa o solo la expresión."),
                            textAreaInput("finv",
                                          "Función F^{-1}(u):",
                                          "function(u) { sqrt(2*u + 1/4) - 1/2 }",
                                          rows = 3)
                          ),
                          
                          # F por trozos
                          conditionalPanel(
                            "input.tipo == 'F por trozos'",
                            helpText("Ingrese los cortes de la distribución. Ejemplo: 0, 1/3, 1"),
                            textInput("cortes", "Cortes:", "0, 1/3, 1"),
                            
                            helpText("Ingrese las inversas por tramo, separadas por ';'"),
                            textAreaInput("inversas",
                                          "Funciones inversas:",
                                          "function(u) { 3*u } ; function(u) { (3*u + 1)/2 }",
                                          rows = 3)
                          ),
                          # Método de aceptación y rechazo
                          conditionalPanel(
                            "input.tipo == 'Método de Aceptación-Rechazo'",
                            helpText("Ingrese la función de densidad, la función envolvente y su inversa G^{-1}(u)."),
                            helpText('Ingrese la función de densidad:'),
                            textAreaInput("densf", "f(x):",
                                          "function(x){ 20*x*(1-x)^3 }", rows = 3),
                            helpText('Ingrese la función envolvente:'),
                            textAreaInput("envolvente", "g(x):",
                                          "function(x){ 0.5 + x }", rows = 3),
                            helpText('Ingrese la inversa de la envolvente'),
                            textAreaInput("ginv", "G^{-1}(u):",
                                          "function(u){sqrt(0.25 + 2*u)-0.5 }",
                                          rows = 3)
                          ),
                          
                          hr(),
                          actionButton("sim_cont", "Simular", icon = icon("dice")),
                          hr(),
                          h4("Resultados de la Simulación"),
                          verbatimTextOutput("cont_code"),
                          plotOutput("histo"),
                          verbatimTextOutput("cont_resumen")
                 )
                 
      )
    )
  )

)

