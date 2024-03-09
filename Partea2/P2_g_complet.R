# -------------------------------Punctul J ------------------------------------------------- 

# Calculul mediei, dispersiei și a momentelor inițiale și centrate pȃnă la ordinul 4(dacă există) 
# atȃt pentru v.a. bidimensională cȃt și pentru v.a. unidimensionale ce o compun . 
# Atunci cȃnd unul dintre momente nu există, se va afișa un mesaj corespunzător către  utilizator.


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
  titlePanel("Media, dispersia, momentele initiale si centrate"),
  sidebarLayout(
    sidebarPanel(
      textInput("functie", "Introduceti functia f(x, y):", "1/5*(x+y+1)"),
      numericInput("x1_val", "Valoarea pentru x1:", 0),
      numericInput("x2_val", "Valoarea pentru x2:", 1),
      numericInput("y1_val", "Valoarea pentru y1:", 0),
      numericInput("y2_val", "Valoarea pentru y2:", 2),
      numericInput("momX_val", "Valoarea pentru momentul lui X:", 2),
      numericInput("momY_val", "Valoarea pentru momentul lui Y:", 2),
      actionButton("calculeaza", "Calculeaza")
    ),
    mainPanel(
      h5("Media:"),
      verbatimTextOutput("rezultat_medie"),
      h5("Dispersia:"),
      verbatimTextOutput("rezultat_dispersie"),
      h5("Momentele initiale si centrate pentru v.a. bidimensionala (01) (10) (20) (02):"),
      verbatimTextOutput("rezultat_momente_XY"),
      h5("Momentul initial si centrat pentru v.a unidimensionala X"),
      verbatimTextOutput("rezultat_momente_X"),
      h5("Momentul initial si centrat pentru v.a unidimensionala Y:"),
      verbatimTextOutput("rezultat_momente_Y")
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
      momX <- input$momX_val
      momY <- input$momY_val
      
      
      # Repartiția marginală a lui X
      f_X <- function(x) {
        #integrate(function(y) f(x, y),-Inf, Inf)$value
        integrate(function(y) f(x, y), y1, y2)$value
      }
      
      # Repartiția marginală a lui Y
      f_Y <- function(y) {
        #integrate(function(x) f(x, y),-Inf, Inf)$value
        integrate(function(x) f(x, y), x1, x2)$value
        
      }
      
      # Media
      mX  <- integrate (Vectorize(function (x) {x * f_X(x)}), x1, x2)$value
      mY  <- integrate (Vectorize(function (y) {y * f_Y(y)}), y1, y2)$value
      mXY <- integral2(Vectorize(function (x ,y ) {(x)*(y)*f(x ,y)}),x1,x2,y1,y2)$Q
        
      # Dispersia
      
      varX  <- integrate (Vectorize(function (x) {(x-mX)*(x-mX)*f_X(x)}),x1,x2)$value
      varY  <- integrate (Vectorize(function (y) {(y-mY)*(y-mY)*f_Y(y)}),y1,y2)$value
      
      
      #Momentele initiale
      mmX<- integrate (Vectorize(function (x) {x^momX * f_X(x)}), x1, x2)$value
      if (!is.finite(mmX)) mmX <- "Nu se poate calcula"
      mmY  <- integrate (Vectorize(function (y) {y^momY * f_Y(y)}), y1, y2)$value
      if (!is.finite(mmY)) mmY <- "Nu se poate calcula"
      
      mXY_01 <-integral2(Vectorize(function (x ,y ) {(x^0)*(y^1)*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mXY_01)) mXY_01 <- "Nu se poate calcula"
      mXY_10 <-integral2(Vectorize(function (x ,y ) {(x^1)*(y^0)*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mXY_10)) mXY_10 <- "Nu se poate calcula"
      mXY_02 <-integral2(Vectorize(function (x ,y ) {(x^0)*(y^2)*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mXY_02)) mXY_02 <- "Nu se poate calcula"
      mXY_20 <-integral2(Vectorize(function (x ,y ) {(x^2)*(y^0)*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mXY_20)) mXY_20 <- "Nu se poate calcula"
      
      
      #Momentele centrate
      mcX <- integrate (Vectorize(function (x) {(x-mX)^momX*f_X(x)}),x1,x2)$value
      if (!is.finite(mcX)) mcX <- "Nu se poate calcula"
      mcY <- integrate (Vectorize(function (y) {(y-mY)^momY*f_Y(y)}),y1,y2)$value
      if (!is.finite(mcY)) mcY <- "Nu se poate calcula"
     
      mcXY_01 <-integral2(Vectorize(function (x ,y ) {(x-mX)^0*(y-mY)^1*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mcXY_01)) mcXY_01 <- "Nu se poate calcula"
      mcXY_10 <-integral2(Vectorize(function (x ,y ) {(x-mX)^1*(y-mY)^0*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mcXY_10)) mcXY_10 <- "Nu se poate calcula"
      mcXY_02 <-integral2(Vectorize(function (x ,y ) {(x-mX)^0*(y-mY)^2*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mcXY_02)) mcXY_02 <- "Nu se poate calcula"
      mcXY_20 <-integral2(Vectorize(function (x ,y ) {(x-mX)^2*(y-mY)^0*f(x ,y)}),x1,x2,y1,y2)$Q
      if (!is.finite(mcXY_20)) mcXY_20 <- "Nu se poate calcula"
     
      
      
      output$rezultat_medie <- renderPrint({
        rezultat_list <- list("Media_X"=mX, "Media_Y"=mY,"Media_XY"=mXY)
        names(rezultat_list) <- c("Media_X", "Media_Y","Media_XY")
        rezultat_list
      })
      
      output$rezultat_dispersie <- renderPrint({
        rezultat_list <- list( "Dispersia_X"=varX, "Dispersia_Y"=varY)
        names(rezultat_list) <- c("Dispersia_X", "Dispersia_Y")
        rezultat_list
      })
      
      output$rezultat_momente_XY <- renderPrint({
        rezultat_list <- list( "M_initial_01_XY"=mXY_01, "M_initial_10_XY"=mXY_10, "M_initial_02_XY"=mXY_02, "M_initial_20_XY"=mXY_20,
                               "M_centrat_01_XY"=mcXY_01, "M_centrat_10_XY"=mcXY_10, "M_centrat_02_XY"=mcXY_02, "M_centrat_20_XY"=mcXY_20)
        names(rezultat_list) <- c("M_initial_01_XY", "M_initial_10_XY", "M_initial_02_XY", "M_initial_20_XY", 
                                  "M_centrat_01_XY", "M_centrat_10_XY", "M_centrat_02_XY", "M_centrat_20_XY")
        rezultat_list
      })
      
      output$rezultat_momente_X <- renderPrint({
        rezultat_list <- list( "M_initial_X"=mmX, "M_centrat_X"=mcX)
        names(rezultat_list) <- c("M_initial_X", "M_centrat_X")
        rezultat_list
      })
      
       output$rezultat_momente_Y <- renderPrint({
        rezultat_list <- list("M_initial_Y"=mmY, "M_centrat_Y"=mcY)
        names(rezultat_list) <- c("M_initial_Y", "M_centrat_Y")
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
