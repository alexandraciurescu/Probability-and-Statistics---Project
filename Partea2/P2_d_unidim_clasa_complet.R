
# -------------------------------Punctul d pentru unidimensionale ------------------------------------------------- 
# crearea unei v.a. continue unidimensionale pentru care se introduce densitatea de repartitie
# utilizatorul va introduce densitatea de repartitie si intervalul [x1,x2]

# Exemplu din materialul de la Variabile aleatoare continue
#      f(x)=3/8*(4*x-2*x*x)    0<x<2 
#           0                  altfel
#      E(X)=1, Var(X)=1/5

library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Creare variabila aleatoare continua unidimensionala"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti densitatea de repartitie f(x):", "3/8*(4*x-2*x*x)"),
      numericInput("x1_val", "Valoarea pentru x1:", 0),
      numericInput("x2_val", "Valoarea pentru x2:", 2),
      actionButton("calculeaza", "Genereaza obiect")
    ),
    mainPanel(
      h5("A fost creat un obiect de tip v.a. continua unidimensionala cu datele membru:"),
      verbatimTextOutput("rezultat"),
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculeaza, {
    tryCatch({
      expr <- parse(text = paste("f <- function(x) {", input$functie, "}"))
      eval(expr)
      
      x1 <- input$x1_val
      x2 <- input$x2_val
      
      
      setClass("ContinuousRV",
               slots = c(
                 pdf = "function",     # densitatea de probabilitate
                 x1 = "numeric",       # capătul inferior al domeniului de definiție
                 x2 = "numeric",       # capătul superior al domeniului de definiție
                 mean = "numeric",     # media
                 variance = "numeric"  # dispersia
               ))
      
      # Constructor pentru clasa
      ContinuousRV <- function(pdf, x1, x2) {
        
        mean_value <- integrate(function(x) x * pdf(x), lower = x1, upper = x2)$value
        variance_value <- integrate(function(x) (x - mean_value)^2 * pdf(x), lower = x1, upper = x2)$value
        
        new("ContinuousRV", pdf = pdf, x1 = x1, x2 = x2, mean = mean_value, variance = variance_value)
      }
      
      X <- ContinuousRV(f,x1,x2)
      
      output$rezultat <- renderPrint({
        rezultat_list <- list("PDF"=X@pdf, "X1"=X@x1, "X2"=X@x2, "Media" = X@mean, "Dispersia" = X@variance)
        names(rezultat_list) <- c("PDF",  "X1", "X2", "Media", "Dispersia")
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
