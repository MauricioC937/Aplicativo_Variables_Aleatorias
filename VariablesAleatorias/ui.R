library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel(
    fluidRow(
      column(width=2),
      column(width=10,h1('Aplicativo Web'))
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
                          conditionalPanel(condition = 'input.simular==1',
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
                 tabPanel("Variables aleatorias Discretas"),
                 tabPanel("Variables aleatorias Continuas"))
    )
  )

)
