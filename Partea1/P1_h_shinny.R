# -------------------------------Punctul h ------------------------------------------------- 
## Test variabile X si Y independente

library(shiny)
library(pracma)
library(discreteRV)

ui <- fluidPage(
  titlePanel("Test variabile independente/corelate"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_val", "Valoarea pentru n:", 4),
      numericInput("m_val", "Valoarea pentru m:", 4),
      actionButton("calculeaza", "Genereaza RV")
    ),
    mainPanel(
      h5("Repartitia comuna generata:"),
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
        
        M<-matrice
        
        #cautam valori egale cu -1 (necompletate)
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
        
        #creare variabila discreta bidimensionala cu ajutorul functiilor din pachetul discreteRV
        XandY = jointRV(outcomes = list(vx, vy), probs = c(M))
        
        X=marginal(XandY, 1)
        Y=marginal(XandY, 2)
        T=independent(X,Y)
        if (T==0)  indep <- "NU"
        else indep<- "DA"
        
        if(length(X)==length(Y))
          { correlation=cor(X,Y)
            if(correlation==0) corelate <- "Necorelate"
            else if(correlation>0) corelate <- "Pozitiv corelate"
                  else corelate <- "Negativ corelate"
            }
        else corelate <- "X si Y au dimensiuni diferite"
        
        output$rezultat <- renderPrint({
          rezultat_list <- list("Matrice"=M, "Independente" = indep, "Corelatie" = corelate)
          names(rezultat_list) <- c("Matrice", "Independente", "Corelatie")
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
