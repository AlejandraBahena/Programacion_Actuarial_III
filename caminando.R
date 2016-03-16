caminando<-function(x){
    a <- 100
    secu <- vector("numeric",0)
    for(i in 1:x){
        
        secu[i] <- a
        length(secu) <- length(secu)+1
       b <- runif(1)
        if(b <=.5){
            a <- a + .5
        } else {
            a <- a - 0.5
        }
        
    }
    secu
    plot(secu, type = "l")
}
