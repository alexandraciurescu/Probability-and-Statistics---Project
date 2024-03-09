**Probability and Statistics Project in R**

**1.**
Fie două variabile aleatoare discrete X şi Y cu repartiţiile de forma de bază.

a)
Construiţi o funcţie frepcomgen care primeşte ca parametri m şi n şi care generează un tabel cu repartiţia comună a v.a. X şi Y incompletă, dar într-o formă în care poate fi completată ulterior.

Observaţie: Se cere la a) să generaţi valorile lui X, valorile lui Y şi suficient de multe valori pentru pi, qj şi respectiv 𝜋ij astfel încât să poată fi determinată repartiţia comună a celor două v.a.

Nota: În construirea algoritmului puteţi începe de la cazul particular m=2 si n=3. Dacă reuşiţi să oferiţi soluţia doar pentru acest caz particular, dar nu şi pentru cazul general veţi primi punctaj parţial.

b)
Construiţi o funcţie fcomplrepcom care completează repartiţia comună generată la punctul anterior(pentru cazul particular sau pentru cazul general).

Nota: În cazul în care nu ştiţi să rezolvaţi punctul a) puteţi construi o funcţie care să determine repartiţia comună pornind de la un exemplu discutat la seminar.

c)
Construiți o funcție frepmarginal care construiește repartițiile marginale pentru X și Y pornind de la repartiția lor comună.

d)
Construiți o funcție fpropcov care aplică proprietățile covarianței pentru calculul acesteia pentru v.a. Z=aX+bY și respectiv T=cX+dY despre X și Y sunt date de intrare.

e)
Construiți o funcție fPcond care calculează probabilitatea condiționată pentru v.a. X și Y pornind de la repartiția comună.

f)
Construiți o funcție fPcomun care calculează o probabilitate legată de perechea (X,Y) pornind de la repartiția comună.

g)
Având la dispoziţie repartiţia comună a v.a. X şi Y de la punctul b) calculaţi:

Cov(5X+9,-3Y-2)
P(0<X<0.8|Y>0.3)
P(X>0.2,Y<1.7)
h)
Pentru exemplul obţinut la punctul b) construiţi două funcţii fverind şi respectiv fvernecor cu ajutorul cărora să verificaţi dacă variabilele X şi Y sunt:

independente
necorelate
i)
Adăugând încă o v.a. Z, propuneți o manieră vizuală de reprezentare a repartiției comune pentru v.a. X, Y și Z. Care ar fi interpretarea repartițiilor marginale în cazul acestei v.a. tridimensionale și cum ar putea fi obținute?

**2.**
Folosind pachetele R shiny (https://shiny.rstudio.com/), animate(https://cran.r-project.org/web/packages/animate/vignettes/introduction.html) și orice alte surse de documentare considerați potrivite construiți un proiect R care să permită lucru cu variabile aleatoare continue bidimensionale. Opțiunile din proiect trebuie să implementeze următoarele funcționalități:

a)
Verificarea posibilității de aplicare a teoremei lui Fubini pentru calculul integralei duble dintr-o funcție f, introdusă de utilizator și afișarea unui mesaj corespunzător către utilizator. Calculul propriu-zis al integralei în această manieră, atunci când este posibil.

b)
Interpretarea geometrică a integralei duble.

c)
Verificarea dacă o funcție cu două variabile f(x,y), introdusă de utilizator este densitate de probabilitate.

d)
Crearea unui obiect de tip variabilă aleatoare continuă pornind de la o densitate de probabilitate introdusă de utilizator. Funcția trebuie să aibă opțiunea pentru variabile aleatoare unidimensionale și respectiv bidimensionale.

e)
Construirea densităților marginale și a celor condiționate pornind de la densitatea comună f(x,y) a două v.a. unidimensionale X și Y.

f)
Reprezentarea grafică a densității și a funcției de repartiție a unei v.a. unidimensionale/bidimensionale pentru diferite valori ale parametrilor repartiției. În cazul în care funcția de repartiție nu este dată într-o formă explicită (ex. repartiția normală) se acceptă reprezentarea grafică a unei aproximări a acesteia. Se obține punctaj suplimentar dacă se realizează o animație care să pună în valoare modificarea funcției reprezentate la schimbarea parametrilor repartiției.

g)
Calculul mediei, dispersiei și a momentelor inițiale și centrate până la ordinul 4 (dacă există) atât pentru v.a. bidimensională cât și pentru v.a. unidimensionale ce o compun. Atunci când unul dintre momente nu există, se va afișa un mesaj corespunzător către utilizator.

h)
Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție continuă unidimensională cunoscută iar g este o funcție continuă precizată de utilizator. Se obține punctaj bonus pentru realizarea aceleiași cerințe și pentru cazul bidimensional.

i)
Crearea unei funcții P care permite calculul diferitelor tipuri de probabilități asociate unei variabile aleatoare continue unidimensionale/bidimension
