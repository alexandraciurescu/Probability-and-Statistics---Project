library(discreteRV)

#punctul c

# Construiți o funcție frepmarginal care construiește repartițiile marginale pentru X și Y 
#pornind de la repartitia lor comuna

frepmarginal <- function(rep_comuna) {
  X <- suppressWarnings(marginal(rep_comuna, 1))
  Y <- suppressWarnings(marginal(rep_comuna, 2))
  # Afisam rezultatele
  print("Repartiția marginală pentru X:")
  print(X)
  
  print("Repartiția marginală pentru Y:")
  print(Y)
  return (list(X = X,  Y = Y))
}

#Exemplu de utilizare:

rep_com_necompletata<-frepcomgen(3,4)
rep_com_necompletata$x
rep_com_necompletata$y
rep_com_completata <- fcomplrepcom(rep_com_necompletata)
valori <- frepmarginal(rep_com_completata)
valori$X
valori$Y
#probX <- (slot(valori$X, "probs"))[[1]] 
#slot(rep_com_completata, "X")





#punctul d
#Construiți o funcție fpropcov care aplică proprietățile covarianței pentru calculul acesteia 
#pentru v.a. Z=aX+bY și respectiv T=cX+dY considerȃnd că toate informațiile necesare 
#despre X și Y sunt date de intrare.

fpropcov <- function(X,Y,a = 1,b = 0,c = 0, d = 1)
{
    Z<-X*a+Y*b
    T<-X*c+Y*d
    cov<-E(Z*T)-E(Z)*E(T)
    return (cov)
}

fpropcov(valori$X, valori$Y, 2, 1, 1, 1)




#e) Construiți o funcție fPcond care calculează probabilitatea condiționată pentru v.a. X și Y
#pornind de la repartiția comună.



fPcond <- function(rep_comuna, valoare_Y, valoare_X=NULL) {
  # Extragem probabilitățile marginale pentru X și Y
  rep_marginale <- frepmarginal(rep_comuna)
  X <- rep_marginale$X
  Y <- rep_marginale$Y
  
  #Cream un pattern pentru o valoare y data pentru (x,y) din repartitia comuna
  #regex_pattern <- paste0(",", valoare_Y, "$") 
  
  #Cautam pozitiile corespunzatoare valorii Y = valoare_Y
  #pozitii <- grep(regex_pattern, outcomes(rep_com_completata))
  
  #selectam probabilitatile
  #probabilitati <- (probs(rep_com_completata))[pozitii]

  #calculam prob_cond  
  #prob_cond <- RV(outcomes(X), probabilitati)
  
  if(is.null(valoare_X))
    prob_cond <- X | (Y == valoare_Y)
  else
    prob_cond <- (X == valoare_X) | (Y == valoare_Y)
  return(prob_cond)
}

#Exemplu utilizare
fPcond(rep_com_completata,730) 
fPcond(rep_com_completata,valoare_X=117, valoare_Y= 730) 




#f) Construiți o funcție fPcomun care calculează o probabilitate legată de perechea (X,Y) 
#pornind de la repartiția comună. 

library(discreteRV)

fPcomun <- function(X, Y, ax = -Inf, bx = Inf, ay = -Inf, by = Inf, eg1 = FALSE, eg2 = FALSE, eg3 = FALSE, eg4 = FALSE, just_eg1 = FALSE, just_eg2 = FALSE) {
  
  # Trebuie să avem măcar o valoare indicată pentru fiecare dintre valorile aleatoare discrete X si Y
  if ((ax == -Inf && bx == Inf) || (ay == -Inf && by == Inf))
    return (-1)
  
  # Adaugare epsilon pentru a trata inclusivitatea
  if (eg1) {
    ax <- ax + 1e-10  
  }
  if (eg2) {
    bx <- bx + 1e-10  
  }
  if (eg3) {
    ay <- ay + 1e-10  
  }
  if (eg4) {
    by <- by + 1e-10  
  }
  
  if (just_eg1 && just_eg2){
    prob <- P( (X == ax) %AND% (Y == ay) )
  } else if (just_eg1){
    if (ax == -Inf){
      return (-1)
    }
    prob <- P( (X == ax) %AND% (Y > ay) %AND% (Y < by) )
  } else if (just_eg2){
    if (bx == -Inf){
      return (-1)
    }
    prob <- P( (X > ax) %AND% (X < bx) %AND% (Y == ay) )
  } else {
    prob <- P( (X > ax) %AND% (X < bx) %AND% (Y > ay) %AND% (Y < by) )
  }
  
  return(prob)
}




#g) Având la dispoziţie repartiţia comună a v.a. X şi Y de la punctul b) calculaţi:

# AandB <- jointRV(outcomes = list(1:3, 0:2), probs = 1:9 / sum(1:9))
# X <- marginal(AandB, 1)
# Y <- marginal(AandB, 2)

rep_marginale <- frepmarginal(rep_com_completata) 
X <- rep_marginale$X
Y <- rep_marginale$Y

#1) Cov(5X+9,-3Y-2)
fpropcov(5*X+9,-3*Y-2)

#2) P(0<X<0.8|Y>0.3)
P(((X> 0) %AND% (X<0.8) ) | (Y > 0.3))

#3) P(X>0.2,Y<1.7)
P((X> 0.2) %AND% (Y <1.7))
fPcomun(X,Y,ax=0.2,by=1.7)



#partea a 2-a

#a) Verificarea posibilitații de aplicare a teoremei lui Fubini pentru calculul integralei duble 
#dintr-o funcție f , introdusă de utilizator și afișarea unui mesaj corespunzător către 
#utilizator. Calculul propriu-zis al integralei ȋn această manieră, atunci cȃnd este posibil. 

library(shiny)

# UI
ui_Fubini <- fluidPage(
  titlePanel("Verificarea Teoremei lui Fubini"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("function", "Introduceți funcția f(x, y):", ""),
      numericInput("x1", "x1:", value = 0),
      numericInput("x2", "x2:", value = 1),
      numericInput("y1", "y1:", value = 0),
      numericInput("y2", "y2:", value = 1),
      actionButton("check_button", "Verifică aplicabilitatea teoremei lui Fubini"),
      textOutput("result_text")
    ),
    mainPanel(
      textOutput("result_x"),
      textOutput("result_y")
    )
  )
)

# Server
server_Fubini <- function(input, output) {
  observeEvent(input$check_button, {
    # Definim funcția
    f <- function(x, y) {
      eval(parse(text = input$'function'), list(x = x, y = y))
    }
    x1 <- input$x1
    x2 <- input$x2
    y1 <- input$y1
    y2 <- input$y2
    
    # Verificăm aplicabilitatea teoremei lui Fubini
    integral_xy <- integrate(Vectorize(function(y) integrate(Vectorize(function(x) f(x, y)), x1, x2)$value), y1, y2)$value
    integral_yx <- integrate(Vectorize(function(x) integrate(Vectorize(function(y) f(x, y)), y1, y2)$value), x1, x2)$value
    
    if (abs(integral_xy - integral_yx) < 1e-10) {
      result <- "Teorema lui Fubini este aplicabilă. Integralele duble sunt egale."
    } else {
      result <- "Teorema lui Fubini nu este aplicabilă. Integralele duble nu sunt egale."
    }
    
    output$result_text <- renderText({
      result
    })
    
    output$result_x <- renderText({
      paste("Rezultatul integralei duble în x:", format(integral_yx, digits = 4))
    })
    
    output$result_y <- renderText({
      paste("Rezultatul integralei duble în y:", format(integral_xy, digits = 4))
    })
  })
}

# Rulam aplicația
shinyApp(ui = ui_Fubini, server = server_Fubini)



#b) Interpretarea geometrică a integralei duble. 

library(shiny)
library(plotly)

ui_Geometric <- fluidPage(
  titlePanel("Interpretarea Geometrica a Integralei Duble"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti functia f(x, y):", ""),
      numericInput("ax", "Valoare pentru ax (opțional):", value = NA),
      numericInput("bx", "Valoare pentru bx (opțional):", value = NA),
      numericInput("ay", "Valoare pentru ay (opțional):", value = NA),
      numericInput("by", "Valoare pentru by (opțional):", value = NA),
      actionButton("deseneaza", "Deseneaza")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server_Geometric <- function(input, output) {
  observeEvent(input$deseneaza, {
    tryCatch({
      expr <- parse(text = paste("f <- function(x, y) {", input$functie, "}"))
      eval(expr)
      
      # Verificam validitatea functiei
      if (!is.function(f)) {
        stop("Introduceti o functie valida.")
      }
      
      # Definim limitele pentru ax, bx, ay, by sau folosește valori implicite
      ax <- ifelse(is.na(input$ax), -1e6, input$ax)
      bx <- ifelse(is.na(input$bx), 1e6, input$bx)
      ay <- ifelse(is.na(input$ay), -1e6, input$ay)
      by <- ifelse(is.na(input$by), 1e6, input$by)
      
      
      # Generam date pentru grafic
      x_vals <- seq(ax, bx, length.out = 100)
      y_vals <- seq(ay, by, length.out = 100)
      z_vals <- outer(x_vals, y_vals, Vectorize(f))
      
      
      # Desenam graficul interactv
      p <- plot_ly(x = x_vals, y = y_vals, z = z_vals, type = "surface") %>%
        layout(scene = list(aspectmode = "cube"))
      
      output$plot <- renderPlotly({
        p
      })
    }, error = function(e) {
      print(e)
    })
  })
}

shinyApp(ui = ui_Geometric, server = server_Geometric)


#f) Reprezentarea grafică a densității și a funcției de repartiție a unei v.a. 
#unidimensionale/bidimensionale pentru diferite valori ale parametrilor repartiției. Ȋn 
#cazul ȋn care funcția de repartiție nu este dată ȋntr-o formă explicită(ex. repartiția 
#normală) se acceptă reprezentarea grafică a unei aproximări a acesteia. 

library(shiny)
library(ggplot2)

# UI
ui_Grafice <- fluidPage(
  titlePanel("Reprezentarea grafică a densității și a funcției de repartiție"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("var_type", "Tip variabilă:", c("Unidimensională", "Bidimensională")),
      textInput("density_function", "Funcția densității (f(x)):", ""),
      numericInput("x1", "Limita inferioară a intervalului x:", value = 0),
      numericInput("x2", "Limita superioară a intervalului x:", value = 1),
      conditionalPanel(
        condition = "input.var_type == 'Bidimensională'",
        numericInput("y1", "Limita inferioară a intervalului y:", value = 0),
        numericInput("y2", "Limita superioară a intervalului y:", value = 1)
      ),
      actionButton("plot_button", "Generează graficul")
    ),
    mainPanel(
      plotOutput("density_plot"),
      plotOutput("cdf_plot")
    )
  )
)

# Server
server_Grafice <- function(input, output) {
  observeEvent(input$plot_button, {
    if (input$var_type == "Unidimensională") {
      x <- seq(input$x1, input$x2, length.out = 100)
      density <- eval(parse(text = input$density_function))
      
      # Calculăm funcția de repartiție unidimensională
      cdf <- rep(0, length(x))
      for (i in 2:length(x)) {
        cdf[i] <- cdf[i - 1] + (x[i] - x[i - 1]) * (density[i] + density[i - 1]) / 2
      }
      cdf <- cdf / max(cdf) # Normalizăm pentru a obține o funcție de repartiție între 0 și 1
      
      data <- data.frame(x, density, cdf)
      
      output$density_plot <- renderPlot({
        ggplot(data, aes(x, density)) + 
          geom_line() + 
          labs(title = "Densitatea")
      })
      
      output$cdf_plot <- renderPlot({
        ggplot(data, aes(x, cdf)) +
          geom_line() +
          labs(title = "Funcția de repartiție")
      })
      
    } else {
      x <- seq(input$x1, input$x2, length.out = 100)
      y <- seq(input$y1, input$y2, length.out = 100)
      z <- outer(x, y, function(x, y) eval(parse(text = input$density_function)))
      df <- data.frame(expand.grid(x = x, y = y), z = as.vector(z))
      
      # Calculăm funcția de repartiție bidimensională
      cdf <- matrix(0, nrow = length(x), ncol = length(y))
      for (i in 2:length(x)) {
        for (j in 2:length(y)) {
          cdf[i, j] <- cdf[i - 1, j] + cdf[i, j - 1] - cdf[i - 1, j - 1] + 
            (x[i] - x[i - 1]) * (y[j] - y[j - 1]) * (z[i, j] + z[i - 1, j] + z[i, j - 1] + z[i - 1, j - 1]) / 4
        }
      }
      cdf <- cdf / max(cdf) # Normalizăm pentru a obține o funcție de repartiție între 0 și 1
      
      output$density_plot <- renderPlot({
        ggplot(df, aes(x, y, z = z)) + 
          geom_contour() + 
          labs(title = "Densitatea")
      })
      
      output$cdf_plot <- renderPlot({
        contour(x, y, cdf, 
                xlab = "x", 
                ylab = "y", 
                main = "Funcția de repartiție")
      })
    }
  })
}

# Rulăm aplicația
shinyApp(ui = ui_Grafice, server = server_Grafice)


#h) Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție 
#continuă unidimensională cunoscută iar g este o funcție continuă precizată de utilizator. 

library(shiny)
library(stats)

ui_MediaDispersie <- fluidPage(
  titlePanel("Calculul Mediei și Dispersiei pentru g(X)"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie_f", "Introduceti functia f(x):", ""),
      textInput("functie_g", "Introduceti functia g(x):", ""),
      numericInput("parametru_a", "Parametrul a:", 0),
      numericInput("parametru_b", "Parametrul b:", 1),
      actionButton("calculeaza", "Calculeaza")
    ),
    mainPanel(
      h3("Rezultate:"),
      verbatimTextOutput("rezultat_medie"),
      verbatimTextOutput("rezultat_dispersie")
    )
  )
)

server_MediaDispersie <- function(input, output) {
  observeEvent(input$calculeaza, {
    tryCatch({
      # Extrage functiile f(x) si g(x) introduse de utilizator
      expr_f <- parse(text = paste("f <- function(x) {", input$functie_f, "}"))
      expr_g <- parse(text = paste("g <- function(x) {", input$functie_g, "}"))
      
      eval(expr_f)
      eval(expr_g)
      
      # Verifica validitatea functiilor f si g
      if (!is.function(f) || !is.function(g)) {
        stop("Introduceti functii valide pentru f(x) si g(x).")
      }
      
      # Extrage parametrii a si b introdusi de utilizator
      a <- input$parametru_a
      b <- input$parametru_b
      
      # Calculeaza media pentru g(X) unde X ~ f(x)
      media_g <- integrate(function(x) g(x) * f(x), lower = a, upper = b)$value
      
      # Calculeaza dispersia pentru g(X) unde X ~ f(x)
      dispersie_g <- integrate(function(x) g(x)^2 * f(x), lower = a, upper = b)$value - media_g^2
      
      output$rezultat_medie <- renderPrint({
        paste("Media g(X):", media_g)
      })
      
      output$rezultat_dispersie <- renderPrint({
        paste("Dispersia g(X):", dispersie_g)
      })
    }, error = function(e) {
      output$rezultat_medie <- renderPrint({
        as.character(e)
      })
      
      output$rezultat_dispersie <- renderPrint({
        as.character(e)
      })
    })
  })
}

shinyApp(ui = ui_MediaDispersie, server = server_MediaDispersie)




#i) Crearea unei funcții P care permite calculul diferitelor tipuri de probabilități asociate 
#unei variabile aleatoare continue unidimensionale/bidimensionale. 

library(shiny)
library(pracma)

ui_Probabilitati <- fluidPage(
  titlePanel("Calculul Probabilitatilor"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("tip_variabila", "Tip variabilă:",
                   choices = c("Unidimensională", "Bidimensională"),
                   selected = "Unidimensională"),
      textInput("functie_densitate", "Introduceti functia densitatii f(x):", ""),
      textInput("valoare_x1", "Valoare x1:", ""),
      textInput("valoare_x2", "Valoare x2:", ""),
      conditionalPanel(
        condition = "input.tip_variabila == 'Bidimensională'",
        textInput("valoare_y1", "Valoare y1:", ""),
        textInput("valoare_y2", "Valoare y2:", "")
      ),
      textInput("valoare_a1", "Valoare a1:", ""),
      textInput("valoare_b1", "Valoare b1:", ""),
      conditionalPanel(
        condition = "input.tip_variabila == 'Bidimensională'",
        textInput("valoare_a2", "Valoare a2:", ""),
        textInput("valoare_b2", "Valoare b2:", "")
      ),
      actionButton("calculeaza_probabilitati", "Calculeaza Probabilitati")
    ),
    mainPanel(
      h3("Rezultate:"),
      textOutput("rezultat_probabilitate_1"),
      textOutput("rezultat_probabilitate_2"),
      textOutput("rezultat_probabilitate_3"),
      textOutput("rezultat_probabilitate_4")
    )
  )
)

server_Probabilitati <- function(input, output) {
  observeEvent(input$calculeaza_probabilitati, {
    tryCatch({
      # Extragem functia densitatii f(x) introdusa de utilizator
      expr_densitate <- parse(text = paste("f <- function(x, y) {", input$functie_densitate, "}"))
      eval(expr_densitate)
      
      # Verificam validitatea functiei densitatii
      if (!is.function(f)) {
        stop("Introduceti o functie valida pentru densitatea f(x).")
      }
      
      # Extragem valorile a1, b1, a2, b2, x1,x2, y1, y2 introduse de utilizator
      x1 <- as.numeric(input$valoare_x1)
      x2 <- as.numeric(input$valoare_x2)
      y1 <- as.numeric(input$valoare_y1)
      y2 <- as.numeric(input$valoare_y2)
      a1 <- as.numeric(input$valoare_a1)
      b1 <- as.numeric(input$valoare_b1)
      a2 <- as.numeric(input$valoare_a2)
      b2 <- as.numeric(input$valoare_b2)
      
      # Initializam a1, a2, b1 și b2 cu valori adecvate
      a1 <- max(a1, x1)
      b1 <- min(b1, x2)
      if (input$tip_variabila == "Bidimensională")
      {
        a2 <- max(a2, y1)
        b2 <- min(b2, y2)
      }
      
      # Tratam valorile infinite explicit
      a1 <- ifelse(a1 == -Inf, -1e10, a1)
      b1 <- ifelse(b1 == Inf, 1e10, b1)
      a2 <- ifelse(a2 == -Inf, -1e10, a2)
      b2 <- ifelse(b2 == Inf, 1e10, b2)
      
      # Calculam probabilitatile asociate variabilei aleatoare X ~ f(x) sau X,Y ~ f(x, y)
      if (input$tip_variabila == "Unidimensională") {
        prob_a1_X_b1 <- integrate(Vectorize(f), a1, b1)$value
        prob_a1_le_X_b1 <- integrate(Vectorize(f), a1, b1)$value
        prob_a1_X_le_b1 <- integrate(Vectorize(f), a1, b1)$value
        prob_a1_le_X_le_b1 <- integrate(Vectorize(f), a1, b1)$value
      } else {
        prob_a1_X_b1 <- integral2(Vectorize(f), a1, b1, a2, b2)$Q
        prob_a1_le_X_b1 <- integral2(Vectorize(f), a1, b1, a2, b2)$Q
        prob_a1_X_le_b1 <- integral2(Vectorize(f), a1, b1, a2, b2)$Q
        prob_a1_le_X_le_b1 <- integral2(Vectorize(f), a1, b1, a2, b2)$Q
      }
      
      output$rezultat_probabilitate_1 <- renderText({
        paste("P(", a1, "< X <", b1, "): ", prob_a1_X_b1)
      })
      
      output$rezultat_probabilitate_2 <- renderText({
        paste("P(", a1, "<= X <", b1, "): ", prob_a1_le_X_b1)
      })
      
      output$rezultat_probabilitate_3 <- renderText({
        paste("P(", a1, "< X <=", b1, "): ", prob_a1_X_le_b1)
      })
      
      output$rezultat_probabilitate_4 <- renderText({
        paste("P(", a1, "<= X <=", b1, "): ", prob_a1_le_X_le_b1)
      })
    }, error = function(e) {
      output$rezultat_probabilitate_1 <- renderText({
        as.character(e)
      })
      output$rezultat_probabilitate_2 <- renderText({
        ""
      })
      output$rezultat_probabilitate_3 <- renderText({
        ""
      })
      output$rezultat_probabilitate_4 <- renderText({
        ""
      })
    })
  })
}

shinyApp(ui = ui_Probabilitati, server = server_Probabilitati)