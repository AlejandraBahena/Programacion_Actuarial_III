Hola 30
Qu� onda? 20
Qu� pedo wey? 10
Qu� pex? 20
Quiubo 20

y <- runif(1)
if(x>.3 && x<.4) {
    y <- "Hola"
} else if(x>0.2 && x<0.3) {
    y <-"Qu� onda?, Quiubo, Qu� pex?"
} else if (x>0.1 && x<0.2) {
    y <- "Qu� pedo wey?"
}
y

# prueba de funcionamiento
n <- 100
mensajes <- vector("character",n)
for(i in 1:n) {
    y <- runif(1)
    if (y <= 0.30){
        mensajes[i] <- "Mensaje 30%"
    } else if (y <= .50){
        mensajes [i] <- "Mensaje 20% a"
    } else if (y <= .70){
        mensajes[i] <- "Mensaje 20% b"
    } else if (y <= .90){
        mensajes[i] <- "Mensaje 20% c"
    } else{
        mensajes[i] <- "Mensaje 10%"
    }
}
plot(table(mensajes))

