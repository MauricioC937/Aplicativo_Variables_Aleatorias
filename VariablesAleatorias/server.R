library(shiny)

# Método multiplicativo
mm <- function(x0,a,m){
  return( (a*x0) %% m)
}
mm(x0=79,a=153,m=97)

mm_sim <- function(x0,a,m,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(k in 1:nsim){
    vec[k+1] <- mm(vec[k],a,m)
  }
  vec <- (vec[-1])/m # retirar la observación x0
  return(vec)
}

### Método mixto
mi <- function(x0,a,m,c){
  return( (a*x0+c) %% m)
}


mi_sim <- function(x0,a,m,c,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(k in 1:nsim){
    vec[k+1] <- mi(vec[k],a,m,c)
  }
  vec <- (vec[-1])/m # retirar la observación x0
  return(vec)
}
#Cuadrados medios
mc <- function(x0,k){
  return(floor((x0^2-floor((x0^2)/(10^(2*k-k/2)))*10^(2*k-k/2))/(10^(k/2))))
}
mc(x0=89,k=2)
mc_sim <- function(x0,k,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(j in 1:nsim){
    vec[j+1] <- mc(vec[j],k)
  }
  vec <- vec[-1]/(10^k) # retiramos x0
  return(vec)
}
### Método Lehmer
ml <- function(x0,n,c){
  return((x0*c-floor(x0*c/10^n)*10^n)-floor(x0*c/10^n))
}

ml(x0=4122,n=4,c=76)
ml_sim <- function(x0,n,c,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(j in 1:nsim){
    vec[j+1] <- ml(vec[j],n,c)
  }
  vec <- vec[-1]/(10^n) # retiramos x0
  return(vec)
}

# VARIABLES ALEATORIAS DISCRETAS

# Método de la transformada inversa
t_inversa <- function(probs, vals, nval){
  sprobs <- cumsum(probs)
  res <- numeric(nval)
  for(j in 1:nval){
    res[j] <- vals[runif(1) < sprobs][1]
  }
  return(res)
}

# Geométrica
x_geometrica <- function(p, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    res[j] <- floor(log(runif(1))/log(1-p)) + 1
  }
  return(res)
}

# Poisson
x_poisson <- function(lambda, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    i <- X <- 0
    Fx <- p <- exp(-lambda)
    U <- runif(1)
    while(U > Fx){
      X <- i
      p <- (lambda*p)/(i+1)
      Fx <- Fx + p
      i <- i+1
    }
    res[j] <- i
  }
  return(res)
}

# Binomial
x_binomial <- function(n, p, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    c <- p/(1-p)
    i <- 0
    Fx <- pr <- (1-p)^n
    U <- runif(1)
    while(U > Fx){
      pr <- c*((n-i)/(i+1))*pr
      Fx <- Fx + pr
      i <- i+1
    }
    res[j] <- i
  }
  return(res)
}

# Define server logic required to draw a histogram
function(input, output, session) {
  
  fx <- reactive({
    expresion <- input$func
    #Construye una función a partir del texto
    function(x){
      eval(parse(text=expresion))
    }
  })
  
  aleatorios <- eventReactive(input$simular,{
    if(input$metodo=='Congruencial multiplicativo'){
      res <- mm_sim(x0=input$x0,a=input$a,m=input$m,nsim=input$num)
    }else if(input$metodo=='Congruencial mixto'){
      res <- mi_sim(x0=input$x0,a=input$a,m=input$m,c=input$c,nsim=input$num)
    }else if(input$metodo=='Cuadrados Medios'){
      res <- mc_sim(x0=input$x0,k=nchar(input$x0),nsim=input$num)
    }else{
      res <- ml_sim(x0=input$x0,n=4,c=2,nsim=input$num)
    }
    res
  })
  
  
  output$code <- renderPrint({
    aleatorios()
  })
  output$grafico <- renderPlot({
    hist(aleatorios(),breaks=10,col='#B22747',main='Números aleatorios')
  })
  output$resumen <- renderPrint({
    summary(aleatorios())
  })
  output$graficofun <- renderPlot({
    xvals <- seq(input$linf,input$lsup,length.out=100)
    yvals <- fx()(xvals)
    
    plot(xvals,yvals,type='l',col='red',main='Función ingresada por el usuario')
  })
  output$integral <- renderPrint({
    hy <- (input$lsup -input$linf)*fx()(input$linf+(input$lsup -input$linf)*aleatorios())
    mean(hy)
  })

  output$integral <- renderPrint({
      f <- fx()
      u <- aleatorios()
    
      a <- if (input$lim_inf_tipo == "fin") input$linf else NA
      b <- if (input$lim_sup_tipo == "fin") input$lsup else NA
      
      if (input$lim_inf_tipo == "fin" && input$lim_sup_tipo == "fin") {
        # Integral en [a, b]
        x <- a + (b - a) * u
        (b - a) * mean(f(x))
        
      } else if (input$lim_inf_tipo == "inf_menor" && input$lim_sup_tipo == "inf_mayor") {
        # Integral en (-∞, ∞)
        x <- tan(pi * (u - 0.5))
        fx_vals <- f(x)
        px <- 1 / (pi * (1 + x^2))
        mean(fx_vals / px)
        
      } else if (input$lim_inf_tipo == "inf_menor" && input$lim_sup_tipo == "fin") {
        # Integral en (-∞, b)
        x <- b - tan(pi * u / 2)
        fx_vals <- f(x)
        px <- 1 / (pi/2 * (1 + (tan(pi * u / 2))^2))
        mean(fx_vals / px)
        
      } else if (input$lim_inf_tipo == "fin" && input$lim_sup_tipo == "inf_mayor") {
        # Integral en [a, ∞)
        x <- a + tan(pi * u / 2)
        fx_vals <- f(x)
        px <- 1 / (pi/2 * (1 + (tan(pi * u / 2))^2))
        mean(fx_vals / px)
      }
    })

  aleatorios_discretos <- eventReactive(input$sim_disc, {
      n <- input$num_disc
      if (input$MetodoDisc == "Transformada Inversa") {
        t_inversa(
          probs = as.numeric(unlist(strsplit(input$probs, ","))),
          vals  = as.numeric(unlist(strsplit(input$vals, ","))),
          nval = n
        )
      } else if (input$MetodoDisc == "Geométrica") {
        x_geometrica(p = input$p_geom, nval = n)
      } else if (input$MetodoDisc == "Poisson") {
        x_poisson(lambda = input$lambda_pois, nval = n)
      } else { 
        x_binomial(n = input$n_bin, p = input$p_bin, nval = n)
      }
    })
    
    output$disc_code <- renderPrint({
      aleatorios_discretos()
    })
    output$disc_grafico <- renderPlot({
      hist(aleatorios_discretos(), 
           col = "orange", border = "black", 
           main = "Distribución Simulada",
           xlab = "Valores generados")
    })
    output$disc_resumen <- renderPrint({
      summary(aleatorios_discretos())
    })
  
}


