a <- 5
b <- 1
c <- vector("numeric",0)
while(a >= 3 && a <= 10){
    moneda <- rbinom(1,1,0.5)
    length(c) <- length(c)
    if(moneda==1){
        a <- a+1
        c[b]<- a
    }else{
        a <- a-1
        c[b]<- a
    }
    b <- b+1
}
c