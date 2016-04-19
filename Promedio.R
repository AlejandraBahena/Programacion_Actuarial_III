m <- 1000 #cantidad de variables por prueba
n <- 100 #catidad de pruebas

f <- function(n,distribucion) {
     prom <- sapply(lapply(rep(m,n), runif), mean)
     hist(prom)
}
f(1000,runif)
