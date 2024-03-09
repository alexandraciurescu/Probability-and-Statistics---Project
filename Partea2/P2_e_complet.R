# -------------------------------Punctul E ------------------------------------------------- 

# Construirea densităților marginale și a celor condiționate pornind de la densitatea 
#  comună f(x,y) a două v.a. unidimensionale X și Y.

#  (1/(2*3.14159265359))*exp( - (x*x+y*y)/2)

library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Construirea densitatilor marginale si a celor conditionale"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti functia f(x, y):", "(1/(2*3.14159265359))*exp(-(x*x+y*y)/2)"),
      numericInput("x1_val", "Valoarea pentru x1:", -1),
      numericInput("x2_val", "Valoarea pentru x2:", 1),
      numericInput("y1_val", "Valoarea pentru y1:", -2),
      numericInput("y2_val", "Valoarea pentru y2:", 3),
      numericInput("x_val", "Densitatea conditionala pentru x:", 1),
      numericInput("y_val", "Densitatea conditionala pentru y:", 1),
      actionButton("calculeaza", "Calculeaza")
    ),
    mainPanel(
      h3("Densitatile marginale:"),
      verbatimTextOutput("rezultat_marginale"),
      h3("Densitatile conditionate:"),
      verbatimTextOutput("rezultat_conditionale")
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
      xc <- input$x_val
      yc <- input$y_val
      
      # Repartiția marginală a lui X
      f_X <- function(x) {
        integrate(function(y) f(x, y), y1, y2)$value
      }
      
      # Repartiția marginală a lui Y
      f_Y <- function(y) {
        integrate(function(x) f(x, y), x1, x2)$value
      }
      
      # Repartiția condiționată a lui X dat fiind y
      f_X_Y <- function(x, y) {
        f(x, y) / f_Y(y)
      }
      
      # Repartiția condiționată a lui Y dat fiind x
      f_Y_X <- function(y, x) {
        f(x, y) / f_X(x)
      }
      
      # Testare functii
      x_values <- seq(x1, x2, by = 0.5)  # serie de valori pentru x
      y_values <- seq(y1, y2, by = 0.5)  # serie de valori pentru y
      
          # Repartiția marginală a lui X pentru fiecare valoare de x
          marginal_X <- sapply(x_values, f_X)
      
           # Repartiția marginală a lui Y pentru fiecare valoare de y
           marginal_Y <- sapply(y_values, f_Y)
      
           # Repartia conditionata a lui X pentru y=1
          cond_x_by_y <- f_X_Y(x_values, yc)
      
          # Repartia conditionata a lui Y pentru x=1
          cond_y_by_x <- f_Y_X(y_values, xc)
    
      output$rezultat_marginale <- renderPrint({
        rezultat_list <- list("Marginal_X" = marginal_X, "Marginal_Y" = marginal_Y)
        names(rezultat_list) <- c("Marginal_X", "Marginal_Y")
        rezultat_list
      })
      
      output$rezultat_conditionale<- renderPrint({
        rezultat_list <- list("Conditional_X" = cond_x_by_y, "Conditional_Y" = cond_y_by_x)
        names(rezultat_list) <- c("Conditional_X", "Conditional_Y")
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
