# -------------------------------Punctul b ------------------------------------------------- 
# Functia fcomplrepcom primeste ca parametru o lista L (cu cele 5 elemente precizate mai sus)
# si returneaza o variabila de tip RV reprezentand repartitia comuna (obtinuta cu functia jointRV)

# Pentru a completa matricea:
# - cautam un element necompletat, fie acesta M[i,j]
# - verificam daca ii putem determina valoarea calculand cate valori lipsesc pe linia i / coloana j
# - daca lipseste doar o singura valoare pe linia/coloana lui atunci ii completam valoarea
# - in completare ne bazam pe repartitiile marginale X si Y

library(shiny)
library(pracma)
library(discreteRV)

ui <- fluidPage(
  titlePanel("Completare matrice repartitie comuna"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_val", "Valoarea pentru n:", 4),
      numericInput("m_val", "Valoarea pentru m:", 4),
      actionButton("calculeaza", "Completeaza matricea")
    ),
    mainPanel(
      h5("Valorile egale cu -1 sunt valorile care lipsesc:"),
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
        
        rezultat1 <- matrice
        
        #cautam valori egale cu -1 (necompletate)
        M <- matrice
        contor = any(M ==-1 )
        
        #cat timp mai sunt valori necompletate
        while (contor > 0) 
        { # preluam pozitiile elementelor egale cu -1 intr-o matrice unde fiecare rand 
          # reprezinta o pereche (linie,coloana) unde se gaseste x
          pozitii = which(M == -1, arr.ind = TRUE)
          
          # incercam sa completam fiecare pozitie necompletata
          for (i in 1:nrow(pozitii)) 
          { #preluam pozitia unei valori lipsa 
            linie = pozitii[i,1]
            coloana = pozitii[i,2]
            
            #verificam daca se poate calcula valoarea elementului de la pozitia (linie,coloana) 
            #trebuie sa avem un singur -1 pe linia sau coloana elementului
            
            numar_elemente_minus_unu = sum(M[linie, ] == -1)
            if(numar_elemente_minus_unu == 1) 
            { suma_elemente_pe_linie = sum(M[linie, ]) + 1
            M[linie,coloana] = probx[linie] - suma_elemente_pe_linie
            }
            else
            { numar_elemente_minus_unu = sum(M[,coloana] == -1)
            if(numar_elemente_minus_unu == 1) 
            { suma_elemente_pe_coloana = sum(M[,coloana]) + 1
            M[linie,coloana] = proby[coloana] - suma_elemente_pe_coloana
            }
            }
          }
          # verificam daca au mai ramas valori egale cu -1 (necompletate)
          contor = any(M == -1) 
        }  
        
        output$rezultat <- renderPrint({
          rezultat_list <- list("Matricea incompleta" = rezultat1, "Matricea completa" = M)
          names(rezultat_list) <- c("Matricea incompleta", "Matricea completa")
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
