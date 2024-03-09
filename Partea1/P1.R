library(discreteRV)

# -------------------------------Punctul A (complet) ------------------------------------------------- 

#Functia primeste ca parametri 2 numere n si m si returneaza o lista cu 5 elemente
# n
# m
# X de tin RV
# Y de tip RV
# M - matrice ce memoreaza repartitia comuna
# Nume campuri lista returnata: n,m,x,y,M

frepcomgen <- function(n, m) {
  
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
  
  print("Matricea dupa generarea initiala:")
  print(matrice)
  
  #Stergem cate o valoare de pe fiecare linie ca sa obtinem o matrice incompleta
  #Generam aleator numarul coloanei de pe care vom elimina elementul de pe fiecare linie
  #Elementele necompletate vor avea valoarea -1
  
  for(i in 1:n)
  { j <- sample(seq(1,m),1)
  matrice[i,j] <- -1
  }
  print("Matricea incompleta (valorile lipsa sunt egale cu -1:")
  print(matrice)
  
  lista_rezultat <- list(n=n, m=m, x = x,  y = y, M = matrice)
  return(lista_rezultat)
}
# -------------------------------Punctul B (complet) -------------------------------------------------

# Functia fcomplrepcom primeste ca parametru o lista L (cu cele 5 elemente precizate mai sus)
# si returneaza o variabila de tip RV reprezentand repartitia comuna (obtinuta cu functia jointRV)

# Pentru a completa matricea:
# - cautam un element necompletat, fie acesta M[i,j]
# - verificam daca ii putem determina valoarea calculand cate valori lipsesc pe linia i / coloana j
# - daca lipseste doar o singura valoare pe linia/coloana lui atunci ii completam valoarea
# - in completare ne bazam pe repartitiile marginale X si Y


fcomplrepcom <- function(L)
{ n <- L$n;  m <- L$m;  X <- L$x;  Y <- L$y;  M <- L$M

#preluam din variabilele aleatoare X si Y probabilitatile si valorile
probX = slot(X, "probs")
probY = slot(Y, "probs")
valuesX = slot(X, "outcomes")
valuesY = slot(Y, "outcomes")

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
            M[linie,coloana] = probX[linie] - suma_elemente_pe_linie
          }
    else
         { numar_elemente_minus_unu = sum(M[,coloana] == -1)
          if(numar_elemente_minus_unu == 1) 
               { suma_elemente_pe_coloana = sum(M[,coloana]) + 1
                 M[linie,coloana] = probY[coloana] - suma_elemente_pe_coloana
               }
          }
      }
     # verificam daca au mai ramas valori egale cu -1 (necompletate)
     contor = any(M == -1) 
   }  

print("Matricea dupa completare:")
print(M)

#creare variabila discreta bidimensionala cu ajutorul functiilor din pachetul discreteRV
XandY = jointRV(outcomes = list(valuesX, valuesY), probs = c(M))

print("Repartitia comuna a variabilelor aleatoare X si Y")

print(XandY)
return(XandY)
}


# -------------------------------Punctul C (complet)-------------------------------------------------

frepmarginal <-function(XandY)
{
  X=marginal(XandY, 1)
  Y=marginal(XandY, 1)
  print(X)
  print(Y)
}

# -------------------------------Punctul D-------------------------------------------------


# -------------------------------Punctul E ------------------------------------------------


# -------------------------------Punctul F -------------------------------------------------


# -------------------------------Punctul G -------------------------------------------------


# -------------------------------Punctul H -------------------------------------------------

# Test variabile X si Y independente
fverind <- function(XandY)
{ 
  X=marginal(XandY, 1)
  Y=marginal(XandY, 2)
  T=independent(X,Y)
  if (T==0) print("NU sunt independente")
  else print("Sunt independente")
}

# Test variabile X si Y necorelate
fvernecor <-function(XandY)
{  
  X=marginal(XandY, 1)
  Y=marginal(XandY, 2)
  
  if(length(X)==length(Y))
      { correlation=cor(X,Y)
        print("Coeficientul de corelatie este: ")
        print(correlation)
        if(correlation==0) print("Necorelate")
        else print("Corelate")
      }
  else print("X si Y au dimensiuni diferite")
  
}


# -------------------------------Punctul I -------------------------------------------------





# --------------------TESTARE---------------------------------------------------------------
L <- frepcomgen(4, 4)  

#print(rep_com_incompleta)
#print(rep_com[3])

XandY <- fcomplrepcom(L)

frepmarginal(XandY)
fverind(XandY)
fvernecor(XandY)
