thermalTime <- function(date,tmin,tmax, sowing, endHarvest, EmergencyDays = 30,parameters = c(0, 12, 24, 35)) 
{
  DataClima<-data.frame(date,tmin,tmax)
  b1 <- 1/(parameters[2] - parameters[1])
  a1 <- -b1 * parameters[1]
  b2 <- 1/(parameters[3] - parameters[4])
  a2 <- -b2 * parameters[4]
  sowing <- as.Date(sowing)
  D1 <- sowing + EmergencyDays
  D2 <- as.Date(endHarvest)
  rango <- DataClima[DataClima[, 1] >= D1 & DataClima[, 1] <= D2, ]
  ndays <- nrow(rango)
  
  minimo <- median(rango$tmin)
  base <- 0
  if (minimo > 10) 
    base <- 2
  TT <- NULL
  peso <- NULL
  day <- NULL
  Y0 <- (rango$tmin[1] + rango$tmax[1])/2
  k <- 1
  if (Y0 < parameters[2]) {
    k <- a1 + b1 * Y0
  }
  if (Y0 > parameters[3]) {
    k <- a2 + b2 * Y0
  }
  if (Y0 < parameters[1] | Y0 > parameters[4]) {
    k <- 0
  }
  peso[1] <- k
  TT[1] <- peso[1] * (Y0 - base)
  day[1] <- EmergencyDays
  for (i in 2:ndays) {
    Y0 <- (rango$tmin[i] + rango$tmax[i])/2
    k <- 1
    if (Y0 < parameters[2]) {
      k <- a1 + b1 * Y0
    }
    if (Y0 > parameters[3]) {
      k <- a2 + b2 * Y0
    }
    if (Y0 < parameters[1] | Y0 > parameters[4]) {
      k <- 0
    }
    peso[i] <- k
    TT[i] <- TT[i - 1] + peso[i] * (Y0 - base)
    day[i] <- day[i - 1] + 1
  }
  tmpDataFrame <- data.frame(date = rango[, 1], tt = TT) # junto en un dataframe la fecha y el vector termal time hallado
  recordTime <- merge( DataClima , tmpDataFrame , all = TRUE ) # junto el df DataClima con el df que contiene el TT, se usa los datos de la columna date para juntar la informacion
  recordTime <- recordTime[,c("date","tt"),drop=FALSE] # elimino todas las columnas solo mantengo date y tt
  recordTime[is.na(recordTime)] <- 0 # reemplazo los valores NA con el valor de cero
  return(recordTime)
}