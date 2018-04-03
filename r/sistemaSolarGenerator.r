
library(mongolite)
metricdb <- mongo("metric", url = "mongodb://localhost:27017/test")

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

cantdias <- 365 * 10
# Inicialido un vector con valores NA, de N dimensiones
filas <- cantdias
columnas <- 1
# Los arrays comienzan en 1
data <- array( dim = c(filas, columnas))
diassequia <- 0
diaslluvia <- 0
diascondicionesoptimas <- 0
diasindeterminado <- 0
perimetro_maximo_triangulo <- 0
dia_perimetro_maximo_triangulo <- 0


for (i in c(1:cantdias)) {
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

           data[i, 1] <- sprintf('{"codigo": "001", "estado": "sequia", "descripcion": "No se pudo calcular linealidad con el sol, vectores horizontales", "dia": "%d", "linealidad": "%s" }', i, ressol)
           metricdb$insert(data[i, 1])
           diassequia <- diassequia + 1

        }
        else {
            if (ressol > 0.95 | ressol < -0.95){
                # Aqui en esta seccion tienen alta linealidad
                # Periodo de sequia
                cat("Linealidad encontrada incluyendo al sol (sequia)", ressol, " POS ", i, "\n")
                
                data[i, 1]  <- sprintf('{"codigo": "002", "estado": "sequia", "descripcion": "Linealidad encontrada incluyendo al sol", "dia": "%d", "linealidad": "%s" }', i, ressol)
                metricdb$insert(data[i, 1])
                diassequia <- diassequia + 1
            }
            else {
                # Determino si los planetas estan alineados entre si (sin el sol)
                df <- data.frame(x = c(vferengix, vbetasoidex, vvulcanox), y = c(vferengiy, vbetasoidey, vvulcanoy))
                res <- cor(df$x, df$y)

                if (is.na(res) ){
                    cat("No se pudo calcular linealidad sin el sol ", res, " POS ", i, "\n")
                     
                     data[i, 1]  <- sprintf('{"codigo": "003", "estado": "indeterminado", "descripcion": "No se pudo calcular linealidad sin el sol, horizontalidad de alguno de los vectores", "dia": "%d", "linealidad": "%s" }', i, res)
                     metricdb$insert(data[i, 1])
                     diasindeterminado <- diasindeterminado + 1
                }
                else {
                    if (res > 0.99 | res < -0.99){
                        # Aqui en esta seccion tienen alta linealidad
                        # Periodo de sequia
                        cat("Linealidad encontrada sin el sol ", res, " POS ", i, "\n")
                        # par(mfrow = c(1, 1))
                        # plot(df$x, df$y, xlab = "X", ylab = "Y")
                        # Sys.sleep(1)
                        
                        data[i, 1]  <- sprintf('{"codigo": "004", "estado": "condiciones optimas de presion y temperatura", "descripcion": "Linealidad encontrada sin incluir al sol", "dia": "%d", "linealidad": "%s" }', i, res)
                        metricdb$insert(data[i, 1])
                        diascondicionesoptimas <- diascondicionesoptimas + 1
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
                        if (abs(areat1) + abs(areat2) + abs(areat3) == abs(areat0)){
                            # Sol se encuentra dentro del triangulo
                            cat("areat0 ", abs(areat0), " areat1 ", abs(areat1), " areat2 ", abs(areat2), " areat3 ", abs(areat3), "\n")
                                                        
                            data[i, 1]  <- sprintf('{"codigo": "005", "estado": "periodo de lluvia", "descripcion": "Sol se encuentra dentro del triangulo", "dia": "%d", "linealidad": "%s" }', i, res)
                            metricdb$insert(data[i, 1])
                            diaslluvia <- diaslluvia + 1
                            
                            # Perimetro maximo del triangulo
                            l01 <- c(vbetasoidex, vbetasoidey) - c(vferengix, vferengiy)
                            perimetro_l01 <- norm (l01, type = "2")
                            l02 <- c(vvulcanox, vvulcanoy) - c(vferengix, vferengiy )
                            perimetro_l02 <- norm (l02, type = "2")
                            l03 <- c(vbetasoidex, vbetasoidey ) - c(vvulcanox, vvulcanoy)
                            perimetro_l03 <- norm (l03, type = "2")
                            perimetro_triangulo <- perimetro_l01 + perimetro_l02 + perimetro_l03

                            cat("perimetro: ", perimetro_triangulo, "\n")
                            if (perimetro_triangulo > perimetro_maximo_triangulo) {
                                perimetro_maximo_triangulo <- perimetro_triangulo
                                dia_perimetro_maximo_triangulo <- i
                            }

                        }
                        else{
                            # Sol se encuentra fuera del triangulo
                            cat("Se descarta el punto", " POS ", i, "\n")

                            data[i, 1]  <- sprintf('{"codigo": "006", "estado": "indeterminado", "descripcion": "Sol se encuentra fuera del triangulo", "dia": "%d", "linealidad": "%s" }', i, res)
                            metricdb$insert(data[i, 1])
                            diasindeterminado <- diasindeterminado + 1
                        }
                    }
                }
            }
        }
        gractualferengi <- gractualferengi - grferengi
        gractualbetasoide <- gractualbetasoide - grbetasoide
        gractualvulcano <- gractualvulcano + grvulcano
}


cat("LLUVIA:  ", diaslluvia, "\n")

cat("SEQUIA:  ", diassequia, "\n")

cat("C.OPTIMAS:  ", diascondicionesoptimas, "\n")

cat("INDETERMINADO:  ", diasindeterminado, "\n")

cat("Perimetro maximo para dia de lluvia: ", perimetro_maximo_triangulo, " dia: ", dia_perimetro_maximo_triangulo, "\n")
