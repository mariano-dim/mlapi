
# Vectores posicion de los tres astros al iniciar, El unico astro que esta en (0, 0) es el sol
vsol <- c(0, 0)
vferengi <- c(500, 0)
vbetasoide <- c(2000, 0)
vvulcano <- c(1000, 0)

grferengi <- 0.01745329
grbetasoide <- 0.05235988
grvulcano <- 0.08726646

radioferengi <- 500
radiobetasoide <- 2000
radiovulcano <- 1000

# Velocidades lineales de los tres astros, en radianes por segundo
# para calcular la velocidad lineal la formula es la siguiente
# v = r , w
# Es el delta del angulo en radianes sobre el delta del tiempo en segundos

vellferengi <- radioferengi * (grferengi / 86400)
vellbetasoide <- radiobetasoide * (grbetasoide / 86400)
vellvulcano <- radiovulcano * (grvulcano / 86400)

# Cuando el vector posicion de los tres astros tiene la misma pendiente estan alineados
# Lo demuestro para el estado inicial
# Como la pendiente de un vector horizontal es cero, todos los vectores estan alineados

# La pendiente para un vector (a,b) es igual a b/a, con a != 0, ya que en ese caso la pendiente 
# es infinita o vertical

# Me piden pronosticar algunas condiciones en los proximos 10 años, Si el bucle fuera por dia, 
# el ciclo se repetiria 365*10 veces totales (3650), La velocidades angulares que me dan son 
# grados cubiertos por dias, por esta razon podria ser mas oportuno un ciclo que sea de 1 grado 
# de crecimiento en cada ciclo, pero en este caso tengo un astro que va en otro sentido,
# Lo debo tener presente a ese hecho,
# Debo actualizar los vectores posicion y calcular las pendientes
# vSol <- c(0, 0) # Siempre queda en el mismo lugar!

vferengix <-  500
vferengiy <-  0
vbetasoidex <- 2000
vbetasoidey <- 0
vvulcanox <- 1000
vvulcanoy <- 0
vsolx <- 0
vsoly <- 0
gractualferengi <- 0
gractualbetasoide <- 0
gractualvulcano <- 0

cantdias <- 100

for (i in c(0:cantdias))
{
        vferengix <-  radioferengi * cos(gractualferengi)
        vferengiy <-  radioferengi * sin(gractualferengi)

        vbetasoidex <- radiobetasoide * cos(gractualbetasoide)
        vbetasoidey <- radiobetasoide * sin(gractualbetasoide)

        vvulcanox <- radiovulcano * cos(gractualvulcano)
        vvulcanoy <- radiovulcano * sin(gractualvulcano)

        # Calculo de linelidad basado en el coeficiente de correlacion lineal

        dfsol <- data.frame(x = c(vsolx, vferengix, vbetasoidex, vvulcanox), y = c(vsoly, vferengiy, vbetasoidey, vvulcanoy))
        ressol <- cor(dfsol$x, dfsol$y)

        if (is.na(ressol) ){
           cat("No se pudo calcular linealidad con el sol ", ressol, " POS ", i, "\n")
        }
        else { 
            if(ressol > 0.95 | ressol < -0.95){
                # Aqui en esta seccion tienen alta linealidad
                # Periodo de sequia
                cat("Linealidad encontrada con el sol", ressol, " POS ", i, "\n")
            }
            else {   
                # Determino si los platentas estan alineados entre si (sin el sol)
                df <- data.frame(x = c(vferengix, vbetasoidex, vvulcanox), y = c(vferengiy, vbetasoidey, vvulcanoy))
                res <- cor(df$x, df$y)

                if (is.na(res) ){
                    cat("No se pudo calcular linealidad sin el sol ", res, " POS ", i, "\n")
                }
                else {
                    if(res > 0.99 | res < -0.99){
                        # Aqui en esta seccion tienen alta linealidad
                        # Periodo de sequia
                        cat("Linealidad encontrada sin el sol ", res, " POS ", i, "\n")
                        par(mfrow = c(1, 1))
                        plot(df$x, df$y, xlab = "X", ylab = "Y")
                        Sys.sleep(1)
                    }
                    else { 
                        # Aqui en esta seccion forman un triangulo
                        # con el sol -posicion (0,0)- dentro del triangulo se forma un periodo de lluvia y es
                        # pico cuando el triangulo es maximo
                        # areat0 = area de P1 P2 P3
                        matt0 <- matrix(c(vferengix, vferengiy, 1, vbetasoidex, vbetasoidey, 1, vvulcanox, vvulcanoy, 1 ), nrow = 3, ncol = 3)
                        areat0 <- 1 / 2 * det(matt0)
                        # areat1 = area P1 P2 P4
                        matt1 <- matrix(c(vferengix, vferengiy, 1, vbetasoidex, vbetasoidey, 1, vsolx, vsoly, 1 ), nrow = 3, ncol = 3)
                        areat1 <- 1 / 2 * det(matt1)
                        # areat1 = area P2 P3 P4
                        matt2 <- matrix(c(vbetasoidex, vbetasoidey, 1, vvulcanox, vvulcanoy, 1, vsolx, vsoly, 1 ), nrow = 3, ncol = 3)
                        areat2 <- 1 / 2 * det(matt2)
                        # areat1 = area P3 P1 P4
                        matt3 <- matrix(c(vvulcanox, vvulcanoy, 1, vferengix, vferengiy, 1, vsolx, vsoly, 1), nrow = 3, ncol = 3)
                        areat3 <- 1 / 2 * det(matt3)
                        if(abs(areat1) + abs(areat2) + abs(areat3) == abs(areat0)){
                            # Sol se encuentra dentro del triangulo
                            cat("areat0 ", abs(areat0), " areat1 ", abs(areat1), " areat2 ", abs(areat2), " areat3 ", abs(areat3), "\n")
                        }
                        else{
                            # Sol se encuentra fuera del triangulo
                            cat("Se descarta el punto", " POS ", i, "\n")
                        }
                    }
                }
            }
        }
        gractualferengi <- gractualferengi - grferengi
        gractualbetasoide <- gractualbetasoide - grbetasoide
        gractualvulcano <- gractualvulcano + grvulcano
}


# Para graficar una circunferencia

# initialize a plot
plot(c(-500,500), c(-500, 500), type = "n")

# prepare "circle data"
radius <- 500
theta <- seq(0, 2 * pi, length = 200)

# draw the circle
lines(x = radius * cos(theta), y = radius * sin(theta))


# Para graficar una serie de puntos sobre el plano
 p1 <- c(1,1)
 p2 <- c(4,1)
 p3 <- c(1,4)
 
plot(x=c(p1[1], p2[1], p3[1]),
    y=c(p1[2], p2[2], p3[2]), 
    ylim=c(-5,5), xlim=c(-5,5))



# Para calcular el vector posicion
x <- r * cos (alfa)
y <- r * sin (alfa)

# Para convertir de dias a segundos
XSeg <- dias*24*60*60

# Para convertir de grados a radianes
XRad <- grados * pi/180


# Para entender el indice de correlacion lineal
El valor del índice de correlación varía en el intervalo [-1,1], indicando el signo el sentido 
de la relación:

Si r = 1, existe una correlación positiva perfecta, El índice indica una dependencia total 
entre las dos variables denominada relación directa: cuando una de ellas aumenta, la otra 
también lo hace en proporción constante,
Si 0 < r < 1, existe una correlación positiva,
Si r = 0, no existe relación lineal, Pero esto no necesariamente implica que las variables 
son independientes: pueden existir todavía relaciones no lineales entre las dos variables,
Si -1 < r < 0, existe una correlación negativa,
Si r = -1, existe una correlación negativa perfecta, El índice indica una dependencia total 
entre las dos variables llamada relación inversa: cuando una de ellas aumenta, la otra 
disminuye en proporción constante,




# Muestra de la salida, perimeros diez elementos

FERENGI   DIA:  0 X POS:  500  Y POS:  0  PENDIENTE:  0 
BETASOIDE DIA:  0 X POS:  2000  Y POS:  0  PENDIENTE:  0 
VULCANO   DIA:  0 X POS:  1000  Y POS:  0  PENDIENTE:  0 

FERENGI   DIA:  1 X POS:  499,9238  Y POS:  8,726202  PENDIENTE:  0,01745506 
BETASOIDE DIA:  1 X POS:  1997,259  Y POS:  104,6719  PENDIENTE:  0,05240778 
VULCANO   DIA:  1 X POS:  996,1947  Y POS:  87,15574  PENDIENTE:  0,08748866 

FERENGI   DIA:  2 X POS:  499,6954  Y POS:  17,44975  PENDIENTE:  0,03492076 
BETASOIDE DIA:  2 X POS:  1989,044  Y POS:  209,0569  PENDIENTE:  0,1051042 
VULCANO   DIA:  2 X POS:  984,8078  Y POS:  173,6482  PENDIENTE:  0,176327 

FERENGI   DIA:  3 X POS:  499,3148  Y POS:  26,16797  PENDIENTE:  0,05240777 
BETASOIDE DIA:  3 X POS:  1975,377  Y POS:  312,8689  PENDIENTE:  0,1583844 
VULCANO   DIA:  3 X POS:  965,9258  Y POS:  258,819   PENDIENTE:  0,2679492 

FERENGI   DIA:  4 X POS:  498,782   Y POS:  34,87823  PENDIENTE:  0,0699268 
BETASOIDE DIA:  4 X POS:  1956,295  Y POS:  415,8234  PENDIENTE:  0,2125566 
VULCANO   DIA:  4 X POS:  939,6926  Y POS:  342,0201  PENDIENTE:  0,3639702 

FERENGI   DIA:  5 X POS:  498,0973  Y POS:  43,57787  PENDIENTE:  0,08748865 
BETASOIDE DIA:  5 X POS:  1931,852  Y POS:  517,6381  PENDIENTE:  0,2679492 
VULCANO   DIA:  5 X POS:  906,3078  Y POS:  422,6182  PENDIENTE:  0,4663076 

FERENGI   DIA:  6 X POS:  497,2609  Y POS:  52,26422  PENDIENTE:  0,1051042 
BETASOIDE DIA:  6 X POS:  1902,113  Y POS:  618,034   PENDIENTE:  0,3249197 
VULCANO   DIA:  6 X POS:  866,0254  Y POS:  500       PENDIENTE:  0,5773502 

FERENGI   DIA:  7 X POS:  496,2731  Y POS:  60,93466  PENDIENTE:  0,1227845 
BETASOIDE DIA:  7 X POS:  1867,161  Y POS:  716,7359  PENDIENTE:  0,3838641 
VULCANO   DIA:  7 X POS:  819,1521  Y POS:  573,5764  PENDIENTE:  0,7002075 

FERENGI   DIA:  8 X POS:  495,134   Y POS:  69,58654  PENDIENTE:  0,1405408 
BETASOIDE DIA:  8 X POS:  1827,091  Y POS:  813,4733  PENDIENTE:  0,4452287 
VULCANO   DIA:  8 X POS:  766,0445  Y POS:  642,7876  PENDIENTE:  0,8390996 

FERENGI   DIA:  9 X POS:  493,8442  Y POS:  78,21722  PENDIENTE:  0,1583844 
BETASOIDE DIA:  9 X POS:  1782,013  Y POS:  907,981   PENDIENTE:  0,5095255 
VULCANO   DIA:  9 X POS:  707,1068  Y POS:  707,1068  PENDIENTE:  1 

FERENGI   DIA: 10 X POS:  492,4039  Y POS:  86,82408  PENDIENTE:  0,176327 
BETASOIDE DIA: 10 X POS:  1732,051  Y POS:  1000      PENDIENTE:  0,5773503 
VULCANO   DIA: 10 X POS:  642,7876  Y POS:  766,0444  PENDIENTE:  1,191754 



FERENGI   DIA:  90 X POS:  0,0001133974  Y POS:  -500  PENDIENTE:  -4409270 
BETASOIDE DIA:  90 X POS:  0,0004392306  Y POS:  2000  PENDIENTE:  4553417 
VULCANO   DIA:  90 X POS:  0,0002339745  Y POS:  1000  PENDIENTE:  4273971 







# Nuevo calculo







vferengix <-  500
vferengiy <-  0
vbetasoidex <- 2000
vbetasoidey <- 0
vvulcanox <- 1000
vvulcanoy <- 0
vsolx <- 0
vsoly <- 0
gractualferengi <- 0
gractualbetasoide <- 0
gractualvulcano <- 0

for (i in c(0:100))
    {
       
        vferengix <-  radioferengi * cos(gractualferengi)
        vferengiy <-  radioferengi * sin(gractualferengi)

        vbetasoidex <- radiobetasoide * cos(gractualbetasoide)
        vbetasoidey <- radiobetasoide * sin(gractualbetasoide)

        vvulcanox <- radiovulcano * cos(gractualvulcano)
        vvulcanoy <- radiovulcano * sin(gractualvulcano)

        # Calculo de linealidad

               cat("Linealidad encontrada " , res, "\n")
               pendferengi <- vferengiy / vferengix
               pendbetasoide <- vbetasoidey / vbetasoidex
               pendvulcano <- vvulcanoy / vvulcanox
               mferengibetasoide <- (vbetasoidey - vferengiy) / (vbetasoidex - vferengix)
               mferengivulcano <- (vvulcanoy - vferengiy) / (vvulcanox - vferengix)
               mbetasoidevulcano <- (vvulcanoy - vbetasoidey) / (vvulcanox - vbetasoidex)
               cat ("POS ", i , " mferengibetasoide: ", mferengibetasoide, " mferengivulcano: ", mferengivulcano, " mbetasoidevulcano: ", mbetasoidevulcano)
               # Calculo la diferencia de pendientes, mediante rangos  
               #cat("FERENGI   DIA: ", i, "X POS: ", vferengix, " Y POS: ", vferengiy,"\n")
               #cat("BETASOIDE DIA: ", i, "X POS: ", vbetasoidex, " Y POS: ", vbetasoidey, "\n")
               #cat("VULCANO   DIA: ", i, "X POS: ", vvulcanox, " Y POS: ", vvulcanoy, "\n")

 
        gractualferengi <- gractualferengi - grferengi
        gractualbetasoide <- gractualbetasoide - grbetasoide
        gractualvulcano <- gractualvulcano + grvulcano 
    }







               #par(mfrow=c(1,1))
               #plot(df$x, df$y, xlab = "X", ylab = "Y")
               #Sys.sleep(1)

               #pendferengi <- vferengiy / vferengix
               #pendbetasoide <- vbetasoidey / vbetasoidex
               #pendvulcano <- vvulcanoy / vvulcanox
               #mferengibetasoide <- (vbetasoidey - vferengiy) / (vbetasoidex - vferengix)
               #mferengivulcano <- (vvulcanoy - vferengiy) / (vvulcanox - vferengix)
               #mbetasoidevulcano <- (vvulcanoy - vbetasoidey) / (vvulcanox - vbetasoidex)
               #cat ("POS ", i , " mferengibetasoide: ", mferengibetasoide, " mferengivulcano: ", mferengivulcano, " mbetasoidevulcano: ", mbetasoidevulcano)
               # Calculo la diferencia de pendientes, mediante rangos  
               #cat("FERENGI   DIA: ", i, "X POS: ", vferengix, " Y POS: ", vferengiy,"\n")
               #cat("BETASOIDE DIA: ", i, "X POS: ", vbetasoidex, " Y POS: ", vbetasoidey, "\n")
               #cat("VULCANO   DIA: ", i, "X POS: ", vvulcanox, " Y POS: ", vvulcanoy, "\n")



