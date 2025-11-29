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
# Binomial Negativa
x_binom_negativa <- function(r,p,nval){
  res <- numeric(nval)
  for(j in 1:nval){
    sum_geom <- 0
    for(i in 1:r){
      U <- runif(1)
      geom <- floor(log(U)/log(1-p)) + 1
      sum_geom <- sum_geom + geom
    }
    res[j] <- sum_geom
  }
  return(res)
}

generar_va <- function(n, tipo, param = list()) {
  if (tipo == "exponencial") {
    lambda <- param$lambda
    U <- runif(n)
    return( -log(U) / lambda )
  }
  if (tipo == "f_fija") {
    Finv <- param$Finv  
    U <- runif(n)
    return( Finv(U) )
  }
  
  if (tipo == "f_trozos") {
    cortes <- param$cortes    
    inversas <- param$inversas
    
    U <- runif(n)
    res <- numeric(n)
    
    for (j in seq_len(n)) {
      u <- U[j]
      tramo <- max(which(cortes <= u))
      res[j] <- inversas[[tramo]](u)
    }
    
    return(res)
  }
  stop("Tipo de distribución continua no reconocido.")
}
# ACEPTACIÓN – RECHAZO
aceptacion_rechazo <- function(nval, fx, gx, g_inv, c){
  res <- numeric(nval)
  i <- 0
  while(i < nval){
    U <- runif(1)
    Y<- g_inv(U)
    U2 <- runif(1)
    if(U2 <= fx(Y)/(c*gx(Y))){
      res[i+1] <- Y
      i <- i+1
    }
  }
  return(res)
}


function(input, output, session) {
  
  fx <- reactive({
    expr <- parse(text = input$func)
    function(x) {
      eval(expr, envir = list(x = x))
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
      } else if (input$MetodoDisc == "Binomial"){
      x_binomial(n = input$n_bin, p = input$p_bin, nval = n)
      } else{
      x_binom_negativa(r=input$r_bin,p=input$p_bneg,nval=n)
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
  # VA Continuas
    datos <- eventReactive(input$sim_cont,{
      
      if (input$tipo == "Exponencial") {
        return(generar_va(input$n, "exponencial",
                          param = list(lambda = input$lambda)))
      }
      
      if (input$tipo == "F fija") {
        
        finv_text <- input$finv
        
        # Si no contiene "function", lo envolvemos automáticamente
        if (!grepl("function", finv_text)) {
          finv_text <- paste0("function(u) { ", finv_text, " }")
        }
        
        Finv <- eval(parse(text = finv_text))
        
        return(generar_va(input$n, "f_fija",
                          param = list(Finv = Finv)))
      }
      if (input$tipo == "F por trozos") {
        cortes <- eval(parse(text = paste("c(", input$cortes, ")")))
        inv_text <- strsplit(input$inversas, ";")[[1]]
        inversas <- lapply(inv_text, function(x) eval(parse(text = x)))
        
        return(generar_va(input$n, "f_trozos",
                          param = list(cortes = cortes,
                                       inversas = inversas)))
      }
      if(input$tipo == "Método de Aceptación-Rechazo"){
      
      fx <- eval(parse(text=input$densf))
      gx <- eval(parse(text=input$envolvente))
      g_inv <- eval(parse(text=input$ginv))
      
      # calcular c = max f(x)/g(x)
      hx <- function(x) fx(x)/gx(x)
      opt <- optimise(hx, interval=c(0,1), maximum=TRUE)
      c <- opt$objective
      return( aceptacion_rechazo(input$n, fx, gx, g_inv, c) )
    }                     
  })
  output$cont_code <- renderPrint({
    datos()
  })
  output$histo <- renderPlot({
    hist(datos(), col = "#F54927",border = "black", 
         main = "Distribución Simulada",
         xlab = "Valores generados")
  })
  output$cont_resumen <- renderPrint({
    summary(datos())
  })
}


