Hola 30
Qué onda? 20
Qué pedo wey? 10
Qué pex? 20
Quiubo 20

y <- runif(1)
  if(x>.3 && x<.4) {
    y <- "Hola"
} else if(x>0.2 && x<0.3) {
    y <-"Qué onda?, Quiubo, Qué pex?"
} else if (x>0.1 && x<0.2) {
    y <- "Qué pedo wey?"
}
y
