# -------------------------------Punctul a ------------------------------------------------- 
#Functia primeste ca parametri 2 numere n si m si returneaza o lista cu 5 elemente
# n
# m
# X de tin RV
# Y de tip RV
# M - matrice ce memoreaza repartitia comuna
# Nume campuri lista returnata: n,m,x,y,M

library(shiny)
library(pracma)
library(discreteRV)

ui <- fluidPage(
  titlePanel("Generare matrice repartitie comuna incompleta"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_val", "Valoarea pentru n:", 4),
      numericInput("m_val", "Valoarea pentru m:", 4),
      actionButton("calculeaza", "Genereaza matricea")
    ),
    mainPanel(
      h5("Valorile lipsa vor avea valoarea -1:"),
      verbatimTextOutput("rezultat")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculeaza, {
    tryCatch({
     
      n <- input$n_val
      m <- input$m_val
        
        # Generare matrice M cu n linii și m coloane, unde suma elementelor este 1 (repartitia comuna)
        valori <- sample(seq(1,100), n*m)
        matrice <- matrix(valori, nrow = n, ncol = m)
        suma_elemente <- sum(matrice)
        matrice <- matrice / suma_elemente
        
        rezultat1 <-matrice
        
        # Generare variabila aleatoare discreta x
        # 1) generare valori
        vx <- sample(1:1000, n, replace = FALSE)
        # 2) calculare probabilitati - suma valorilor de pe fiecare linie din repartitia comuna M
        probx <- rowSums(matrice)
        # 3) creare variabila aleatoare x
        x <- RV (vx, probx)
        
        # Generare variabila aleatoare discreta y
        # 1) generare valori
        vy <- sample(1:1000, m, replace = FALSE)
        # 2) calculare probabilitati - suma valorilor de pe fiecare coloana din repartitia comuna M
        proby <- colSums(matrice)
        # 3) creare variabila aleatoare y
        y <- RV (vy, proby)
        
        #Stergem cate o valoare de pe fiecare linie ca sa obtinem o matrice incompleta
        #Generam aleator numarul coloanei de pe care vom elimina elementul de pe fiecare linie
        #Elementele necompletate vor avea valoarea -1
        
        for(i in 1:n)
        { j <- sample(seq(1,m),1)
        matrice[i,j] <- -1
        }
        
        rezultat2 <- matrice
        
        output$rezultat <- renderPrint({
          rezultat_list <- list("Matricea completa" = rezultat1, "Matricea incompleta" = rezultat2)
          names(rezultat_list) <- c("Matricea completa", "Matricea incompleta")
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
