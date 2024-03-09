# -------------------------------Punctul J ------------------------------------------------- 

# Calculul covarianței și coeficientului de corelație pentru două variabile aleatoare 
# continue. Trebuie să folosiți densitatea comună a celor două variabile aleatoare!)

#  Exemplu 1 pentru teste
#  f(x,y) = 2*x*y*exp(-(x^2+y^2)), x>0, y>0
#           0, in rest
#  E(x)=E(y)=sqrt(pi)/4=0.11
#  E(x^2)=1/2
#  Var(X)=Var(Y)=1/2-pi/16


# Exemplu 2 pentru teste
# f(x,y)=1/5*(x+y+1)  x din [0,1], y din [0,2]
#          0 in rest
# E(x)=8/15=0.53
# E(y)=17/15=1.13
# Var(x)= 37/450=0.08
# Var(y)=71/225=0.31
# C(x,y)=-1/225=-0.04
# Cor(x,y)=-0.02



library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Covarianta si coeficientul de corelatie pentru doua variabile aleatoare continue"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti functia f(x, y):", "1/5*(x+y+1)"),
      numericInput("x1_val", "Valoarea pentru x1:", 0),
      numericInput("x2_val", "Valoarea pentru x2:", 1),
      numericInput("y1_val", "Valoarea pentru y1:", 0),
      numericInput("y2_val", "Valoarea pentru y2:", 2),
      actionButton("calculeaza", "Calculeaza")
    ),
    mainPanel(
      h3("Rezultat:"),
      verbatimTextOutput("rezultat")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculeaza, {
    tryCatch({
      expr <- parse(text = paste("f <- function(x, y) {", input$functie, "}"))
      eval(expr)
      
      x1 <- input$x1_val
      x2 <- input$x2_val
      y1 <- input$y1_val
      y2 <- input$y2_val
      
      
      # Repartiția marginală a lui X
      f_X <- function(x) {
        integrate(function(y) f(x, y), y1, y2)$value
      }
      
      # Repartiția marginală a lui Y
      f_Y <- function(y) {
        integrate(function(x) f(x, y), x1, x2)$value
        
      }
      
      # Mediile celor 2 variabile aleatoare
      
      mX <- integrate (Vectorize(function (x) {x * f_X(x)}), x1, x2)$value
      mY <- integrate (Vectorize(function (y) {y * f_Y(y)}), y1, y2)$value
      
      # Dispersiile celor 2 variabile aleatoare
      
      varX <- integrate (Vectorize(function (x) {(x-mX)*(x-mX)*f_X(x)}),x1,x2)$value
      varY <- integrate (Vectorize(function (y) {(y-mY)*(y-mY)*f_Y(y)}),y1,y2)$value
      
      # Covarianta
      #cov_XY <- integral2(Vectorize(function (x ,y ) {(x-mX)*(y-mY)*f(x ,y)}),y1,y2,x1,x2)$Q
      cov_XY <- integral2(Vectorize(function (x ,y ) {(x-mX)*(y-mY)*f(x ,y)}),x1,x2,y1,y2)$Q
  
      
      #Coeficientul de corelatie
      cor_XY <- cov_XY/ (sqrt(varX)*sqrt(varY))
      
      
    
      output$rezultat <- renderPrint({
        rezultat_list <- list("Ex"=mX, "Ey"=mY, "Varx"=varX, "Vary"=varY, "Covarianta" = cov_XY,"Corelatia"=cor_XY)
        names(rezultat_list) <- c("Ex","Ey","Varx", "Vary", "Covarianta", "Corelatia")
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
