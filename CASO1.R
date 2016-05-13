#FUNCIÓN "MEDIACONTAMINANTE"
mediacontaminante  <- function(directorio, contaminante, id = 1:332) {
    data = lapply(id, function(i) read.csv(paste(directorio, "/", formatC(i,width = 3, flag = "0"), ".csv", sep = ""))[[contaminante]])
    
    return(mean(unlist(data), na.rm = TRUE))
}

#FUNCIÓN "COMPLETOS"
completos <- function(directorio, id = 1:332) {
    f <- function(i) {
        data = read.csv(paste(directorio, "/", formatC(i, width = 3, flag = "0"), 
                              ".csv", sep = ""))
        sum(complete.cases(data))
    }
    nobs = sapply(id, f)
    return(data.frame(id, nobs))
}

#FUNCIÓN "CORR"
corr <- function(directorio, horizonte=0) {
    
    df = completos(directorio)
    ids = df[df["nobs"] > horizonte, ]$id
    corrr = numeric()
    for (i in ids) {
        
        newRead = read.csv(paste(directorio, "/", formatC(i, width = 3, flag = "0"), 
                                 ".csv", sep = ""))
        dff = newRead[complete.cases(newRead), ]
        corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    return(corrr)
}