#FUNCIÓN "MEJOR"
mejor<-function(estado, resultado) {
    data <- read.csv("outcome-of-care-measures.csv")
    estados <- levels(data[, 7])[data[, 7]] 
    CodigoEstado <- FALSE
    
    for (i in 1:length(estados)) {
        if (estado == estados[i]) {
            CodigoEstado <- TRUE
        }
    }
    
    if (!CodigoEstado) {
        stop ("estado invalido")
    }
    
    if (!((resultado == "infarto") | (resultado == "falla")
          | (resultado == "neumonia"))) {
        stop ("resultado invalido")
    }
    
    col <- if (resultado == "infarto") {
        11 
    } else if (resultado == "falla") {
        17 
    } else {
        23 
    }
    
    data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
    data[, 2] <- as.character(data[, 2]) 
    
    DataEstado <- data[grep(estado, data$State), ]
    
    Ordenar_Data <- DataEstado[order(DataEstado[, col], DataEstado[, 2], na.last = NA), ]
    Ordenar_Data[1, 2]
}

mejor("TX", "infarto") 
mejor("TX", "falla") 
mejor("MD", "infarto")
mejor("MD", "neumonia")
mejor("BB", "infarto")
mejor("NY", "infartiqui")

#FUNCIÓN "RANKHOSPITAL"
rankhospital <- function(estado, resultado, num = "mejor"){
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    x <- levels(factor(data[,7]))
    v <- c("infarto", "falla", "neumonia")
    
    if (estado %in% x == F){
        stop("estado inválido")
        break
    }
    if (resultado == "infarto") r <- 11
    else if (resultado == "falla") r <- 17
    else if (resultado == "neumonia") r <- 23
    else if (resultado %in% v == F){
        stop("resultado inválido")
        break
    }
    mydata <- data[data$State == estado,]
    mnd <- mydata[,c(2,r)]
    if (sum(mnd[,2]=="Not Available") < 1) {
        
        out <- mnd[order(as.numeric(final[,2])),]
        if (num == "mejor") num <- 1
        else if (num == "peor") num <- nrow(out)
        else if (num > nrow(out)) {
            stop(return(NA))
        }
        i <- 0
        while (out[i+1,2] != out[num,2]){
            i <- i + 1
        }
        dif <- num - i
        out2 <- out[which(out[,2] == out[num,2]),]
        fo <- out2[order(out2[,1]),]
        fo[dif,1] 
    }
    
    else  {
        final <- mnd[- grep("Not", mnd[,2]),]
        out <- final[order(as.numeric(final[,2])),]
        if (num == "mejor") num <- 1
        else if (num == "peor") num <- nrow(out)
        else if (num > nrow(out)) {
            stop(return(NA))
        }
        i <- 0
        while (out[i+1,2] != out[num,2]){
            i <- i + 1
        }
        dif <- num - i
        out2 <- out[which(out[,2] == out[num,2]),]
        fo <- out2[order(out2[,1]),]
        fo[dif,1]
    }
}
rankhospital("TX", "falla", 4)
rankhospital("MD", "infarto", "peor")
rankhospital("MN", "infarto", 5000)

#FUNCIÓN "RANKINGCOMPLETO"
rankingcompleto <- function(resultado="infarto",num="mejor"){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    options(warn = -1)
    if(resultado == "infarto"){
        da <- split(outcome[c(2,7,11)],outcome[c(2,7,11)]$State)
    }else if(resultado=="falla"){  
        da <- split(outcome[c(2,7,17)],outcome[c(2,7,17)]$State)
    }else if(resultado=="neumonia"){
        da <- split(outcome[c(2,7,23)],outcome[c(2,7,23)]$State)
    }
    Hosp <- c()
    for(j in 1:54){
        H <- da[j][[1]][,1]
        Rate <- as.numeric(da[j][[1]][,3])
        x <- na.omit(data.frame(H,Rate))
        index <- with(x,order(Rate,H))
        x <- x[index,][1]
        y <- nrow(x)
        if(num=="mejor"){
            Hosp <- c(Hosp,as.character(x[1,1]))
        }else if(num=="peor"){
            Hosp <- c(Hosp,as.character(x[y,1]))
        }else if(0<num & num<=y ){
            Hosp <- c(Hosp,as.character(x[num,1]))
        }else if (y<num){
            Hosp <- c(Hosp,as.character(x[y+1,1]))
        } 
    }
    Estado <- sort(unique(outcome$State))
    data.frame(Hosp,Estado)
}

head(rankingcompleto("infarto",20),10)
tail(rankingcompleto("neumonia","peor"),3)
tail(rankingcompleto("falla","mejor"),10)