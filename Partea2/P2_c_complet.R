# -------------------------------Punctul c ------------------------------------------------- 

# test densitate de probabilitate pentru o functie cu 2 parametri (x,y)
#  - trebuie sa verificam sa fie pozitiva si probabilitatea totala sa fie 1


# Exemple pentru teste
# f(x,y)=4*x*y,  0<X<1, 0<y<1
#          0 in rest

# f(x,y)= 1/5*(x+y+1),  0<=X<=1, 0<=y<=2
#         0 in rest



library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Test densitate de probabilitate"),
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
      
      # test valori pozitive pe domeniul [x1,x2] x [y1,y2]
      ok <- 1
      for(i in seq(x1,x2, 0.01))
        for(j in seq (y1,y2, 0.01))
                if( f(i,j) < 0) ok<-0
      
      
      #test probabilitatea totala egala cu 1
      
     integral <- integral2(Vectorize(f), x1, x2, y1, y2)$Q
     if (abs(integral-1) > 0.1) ok <- 0
      
      
     if (ok==1) rezultat <- "Functia este densitate de probabilitate"
     else rezultat <- "Functia nu este densitate de probabilitate"
      
     
     output$rezultat <- renderPrint({
        rezultat
      })
    }, error = function(e) {
      output$rezultat <- renderPrint({
        as.character(e)
      })
    })
  })
}

shinyApp(ui = ui, server = server)
