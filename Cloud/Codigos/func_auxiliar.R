# Conjunto de funções que desconsideram valores NAs
sum2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, sum(vet, na.rm = TRUE))} 
median2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, median(vet, na.rm = TRUE))}
mean2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, mean(vet, na.rm = TRUE))}
max2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, max(vet, na.rm = TRUE))}
min2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, min(vet, na.rm = TRUE))}
sd2 = function(vet){sd(vet, na.rm = TRUE)}
var2 = function(vet){var(vet, na.rm = TRUE)}
length2 = function(vet){length(vet)-sum(is.na(vet))}
q1_2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, quantile(vet, 0.25, na.rm = TRUE))}
q3_2 = function(vet){ifelse(sum(!is.na(vet))==0, NA, quantile(vet, 0.75, na.rm = TRUE))}
formatar = function(vet){formatC(vet, digits = 4, format = "fg", decimal.mark = ",", big.mark = ".")}
formatar2 = function(vet, dig=4){formatC(vet, digits = dig, format = "fg", decimal.mark = ",", big.mark = ".")}


median3 = function(vet){
  if(sum(!is.na(vet))==0){
    return(NA)
  } 
  else {
    vet = vet[which(!is.na(vet))]
    vet = vet[order(vet)]
    return(vet[round(length(vet)/2)])
  }
}
max3 = function(vet){
  if(sum(!is.na(vet))==0){
    return(NA)
  } 
  else {
    vet = vet[which(!is.na(vet))]
    vet = vet[order(vet)]
    return(vet[round(length(vet))])
  }
}
min3 = function(vet){
  if(sum(!is.na(vet))==0){
    return(NA)
  } 
  else {
    vet = vet[which(!is.na(vet))]
    vet = vet[order(vet)]
    return(vet[1])
  }
}
q1_3 = function(vet){
  if(sum(!is.na(vet))==0){
    return(NA)
  } 
  else {
    vet = vet[which(!is.na(vet))]
    vet = vet[order(vet)]
    return(vet[round(length(vet)*0.25)])
  }
}
q3_3 = function(vet){
  if(sum(!is.na(vet))==0){
    return(NA)
  } 
  else {
    vet = vet[which(!is.na(vet))]
    vet = vet[order(vet)]
    return(vet[round(length(vet)*0.75)])
  }
}
