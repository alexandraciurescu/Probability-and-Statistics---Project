
# -------------------------------Punctul d pentru bidimensionale ------------------------------------------------- 
# crearea unei v.a. continue bidimensionale pentru care se introduce densitatea de repartitie
# utilizatorul va introduce densitatea de repartitie si intervalele [x1,x2], [y1,y2]

# Exemplu pentru teste
# f(x,y)=1/5*(x+y+1)  x din [0,1], y din [0,2]
#          0 in rest
# E(x)=8/15=0.53
# E(y)=17/15=1.13
# Var(x)= 37/450=0.08
# Var(y)=71/225=0.31
# Cov(x,y)=-1/225=-0.04
# Cor(x,y)=-0.02


library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Creare variabila aleatoare continua bidimensionala"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti densitatea de repartitie f(x):", "1/5*(x+y+1)"),
      numericInput("x1_val", "Valoarea pentru x1:", 0),
      numericInput("x2_val", "Valoarea pentru x2:", 1),
      numericInput("y1_val", "Valoarea pentru y1:", 0),
      numericInput("y2_val", "Valoarea pentru y2:", 2),
      actionButton("calculeaza", "Genereaza obiect")
    ),
    mainPanel(
      h5("A fost creat un obiect de tip v.a. continua bidimensionala cu datele membru:"),
      verbatimTextOutput("rezultat")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculeaza, {
    tryCatch({
      expr <- parse(text = paste("f <- function(x,y) {", input$functie, "}"))
      eval(expr)
      
      x1 <- input$x1_val
      x2 <- input$x2_val
      y1 <- input$y1_val
      y2 <- input$y2_val
      
      setClass("ContinuousRV2",
               slots = c(
                 pdf = "function", # densitatea de probabilitate
                 x1 = "numeric", # capătul inferior pentru X
                 x2 = "numeric", # capătul superior pentru X
                 y1 = "numeric", # capătul inferior pentru Y
                 y2 = "numeric", # capătul superior pentru Y
                 mean = "numeric", #media pentru XY
                 mean_x = "numeric",   # media pentru X
                 mean_y = "numeric",   # media pentru Y
                 
                 variance_x = "numeric", # dispersia X
                 variance_y = "numeric", # dispersia Y
                 covariance = "numeric", # covarianta
                 correlation = "numeric" # coeficientul de corelatie
               ))
      
      # Constructor pentru clasa
      ContinuousRV2 <- function(pdf, x1, x2, y1, y2) {
      
        # Densitatea marginală a lui X
        f_X <- function(x) { integrate(function(y) f(x, y), y1, y2)$value}
      
        # Repartiția marginală a lui Y
        f_Y <- function(y) { integrate(function(x) f(x, y), x1, x2)$value}
      
        # Media
        mX  <- integrate (Vectorize(function (x) {x * f_X(x)}), x1, x2)$value
        mY  <- integrate (Vectorize(function (y) {y * f_Y(y)}), y1, y2)$value
        mXY <- integral2(Vectorize(function (x ,y ) {(x)*(y)*f(x ,y)}),x1,x2,y1,y2)$Q
        
        # Dispersia
        varX  <- integrate (Vectorize(function (x) {(x-mX)*(x-mX)*f_X(x)}),x1,x2)$value
        varY  <- integrate (Vectorize(function (y) {(y-mY)*(y-mY)*f_Y(y)}),y1,y2)$value
        
        
        # Covarianta
        cov_XY <- integral2(Vectorize(function (x ,y ) {(x-mX)*(y-mY)*f(x ,y)}),x1,x2,y1,y2)$Q
        
        #Coeficientul de corelatie
        cor_XY <- cov_XY/ (sqrt(varX)*sqrt(varY))
      
        new("ContinuousRV2", pdf = pdf, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
            mean = mXY, mean_x = mX, mean_y = mY,  
            variance_x = varX, variance_y = varY,
            covariance = cov_XY, correlation = cor_XY)
      }
      
      X <-ContinuousRV2(f,x1,x2,y1,y2)
      
      output$rezultat <- renderPrint({
        rezultat_list <- list("PDF"=X@pdf, "X1"=X@x1, "X2"=X@x2, "Y1"=X@y1, "Y2"=X@y2,
                              "E(X,Y)" = X@mean, "E(X)" = X@mean_x, "E(Y)" = X@mean_y,
                               "Var(X)" = X@variance_x, "Var(Y)" = X@variance_y,
                              "Covarianta" = X@covariance, "Corelatia" = X@correlation)
        names(rezultat_list) <- c("PDF", "X1", "X2", "Y1", "Y2", "E(X,Y)", "E(X)", "E(Y)",
                               "Var(X)", "Var(Y)", "Covarianta", "Corelatia")
        rezultat_list
      })
      
    }, error = function(e) {
      output$rezultat <- renderPrint({
        as.character(e)
      })
    })
  })
}

shinyApp(ui = ui, server = server)
