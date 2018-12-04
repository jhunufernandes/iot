carregaSensores = function(){
  return(read.table("Dados/Configuracao/listaSensor.csv", sep = ";", dec = ",", header = T))
}

classificaTemperatura = function(temperatura){
  quali2 = factor(cut(temperatura, c(-63, 0, 18, 28, 36, 100),
                      c("Muito Frio", "Frio", "Agradável","Quente", "Muito Quente"), include.lowest = TRUE), ordered = T)
  return(quali2)
}

classificaPoluicao = function(poluicao){
  inter = (300 - 0)/5
  m = 0
  quali1 = factor(cut(poluicao, c(m, m+inter, m+inter*2, m+inter*3, m+inter*4, m+inter*5),
                      c("Ótimo", "Bom", "Regular","Ruim", "Péssimo"), include.lowest = TRUE), ordered = T)
  return(quali1)
}

corPoluicao = function(qualiPoluicao){
  return(ifelse(qualiPoluicao == "Ótimo", "lime",
                ifelse(qualiPoluicao == "Bom", "green",
                       ifelse(qualiPoluicao == "Regular", "yellow",
                              ifelse(qualiPoluicao == "Ruim", "orange",
                                     ifelse(qualiPoluicao == "Péssimo", "red","light-blue"))))))
}

corTemperatura = function(qualiTemperatura){
  return(ifelse(qualiTemperatura == "Muito Frio", "blue",
                ifelse(qualiTemperatura == "Frio", "light-blue",
                       ifelse(qualiTemperatura == "Agradável", "green",
                              ifelse(qualiTemperatura == "Quente", "orange",
                                     ifelse(qualiTemperatura == "Muito Quente", "red","black"))))))
}

diaSemana = function(vet){
  sem = weekdays(vet)
  sem = gsub("Sunday", "domingo", sem)
  sem = gsub("Monday", "segunda", sem)
  sem = gsub("Tuesday", "terça", sem)
  sem = gsub("Wednesday", "quarta", sem)
  sem = gsub("Thursday", "quinta", sem)
  sem = gsub("Friday", "sexta", sem)
  sem = gsub("Saturday", "sábado", sem)
  sem = factor(sem, levels = c("domingo", "segunda", "terça", "quarta", "quinta", "sexta", "sábado"))
  return(sem)
}

toData = function(ano, mes, dia, hora, minuto, segundo){
  ISOdatetime(year = ano, month = mes,  day = dia,  hour = hora,  min = minuto, sec =  segundo, tz = "America/Sao_Paulo")
}

transfConsumoMedidoEvento = function(sensor, inicio = now()-days(7), fim = now()){
  diretorio = paste0("Dados/Desagregado/Sensores/", sensor)
  tabTransf = data.frame()
  
  for (i in dir(diretorio)) {
    dia = as.numeric(substr(i, 1, 2))
    mes = as.numeric(substr(i, 3, 4))
    ano = as.numeric(substr(i, 5, 8))
    dataArq = date(paste(ano, mes, dia, sep ="-"))
    
    if(dataArq >= date(inicio) & dataArq <= date(fim)){
      arq = paste0(diretorio,"/" ,i)
      tab = read.table(arq, sep = ";", dec = ",", header = F)
      names(tab) = c("HORA", "MINUTO", "SEGUNDO", "LATITUDE", "LONGITUDE", "POLUICAO", "TEMPERATURA")
      ANO = rep(ano, nrow(tab))
      MES = rep(mes, nrow(tab))
      DIA = rep(dia, nrow(tab))
      HORA_REF = toData(ANO, MES, DIA, tab$HORA, tab$MINUTO, tab$SEGUNDO)
      tabTransf = rbind(tabTransf, data.frame(HORA_REF, 
                                              LATITUDE = tab$LATITUDE,
                                              LONGITUDE = tab$LONGITUDE,
                                              POLUICAO = tab$POLUICAO, 
                                              TEMPERATURA = tab$TEMPERATURA))
    }
  }
  return(tabTransf)
}

transfConsumoMedidoHora = function(sensor, inicio = now()-days(7), fim = now()){
  inicio = date(inicio) 
  fim = date(fim)
  
  tab = transfConsumoMedidoEvento(sensor, inicio, fim)
  tabHora = aggregate(tab[,c("POLUICAO", "TEMPERATURA")], 
                      list(HORA_REF = toData(year(tab$HORA_REF), month(tab$HORA_REF), day(tab$HORA_REF), hour(tab$HORA_REF), 0, 0)), 
                      mean2)

  if(fim == date(now())){
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), day(inicio), 0 , 0, 0),
                          toData(year(fim), month(fim), day(fim), hour(now()) , 0, 0), "hour")
  }else{
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), day(inicio), 0 , 0, 0),
                          toData(year(fim), month(fim), day(fim), 23 , 0, 0), "hour")
  }
  
  tabHora = merge(tabHora, data.frame(HORA_REF), by = "HORA_REF", all.y = T)
  return(tabHora)
}

transfConsumoMedidoDia = function(sensor, inicio = now()-days(7), fim = now()){
  inicio = date(inicio) 
  fim = date(fim)
  
  tab = transfConsumoMedidoHora(sensor, inicio, fim)
  tabDia = aggregate(tab[,c("POLUICAO", "TEMPERATURA")], 
                      list(HORA_REF = toData(year(tab$HORA_REF ), month(tab$HORA_REF ), day(tab$HORA_REF ), 0, 0, 0)), 
                      mean2)

  return(tabDia)
}

transfConsumoMedidoMes = function(sensor, inicio = now()-days(7), fim = now()){
  inicio = date(inicio) 
  fim = date(fim)
  
  tab = transfConsumoMedidoHora(sensor, inicio, fim)
  tabMes = aggregate(tab[,c("POLUICAO", "TEMPERATURA")], 
                      list(HORA_REF = toData(year(tab$HORA_REF ), month(tab$HORA_REF ), 1, 0, 0, 0)), 
                      mean2)
  return(tabMes)
}

agregaConsumoMedidoHora = function(sensor, inicio = now()-days(7), fim = now(), metodo, variavel, tabelaDados = NULL){
  #inicio = date(inicio) 
  #fim = date(fim)
  
  if(is.null(tabelaDados)){
    tab = transfConsumoMedidoEvento(sensor, inicio, fim)
  }else{
    tab = subset(tabelaDados, tabelaDados$HORA_REF >= inicio & tabelaDados$HORA_REF <= fim)
  }
  tabHora = aggregate(tab[,c(variavel)], 
                      list(HORA_REF = toData(year(tab$HORA_REF), month(tab$HORA_REF), day(tab$HORA_REF), hour(tab$HORA_REF), 0, 0)), 
                      metodo)
  LATITUDE = c()
  LONGITUDE = c()
  HORARIO = rep(toData(0,0,0,0,0,0),nrow(tabHora))
  for(i in 1:length(tabHora$HORA_REF)){
    aux = subset(tab, date(tab$HORA_REF) == date(tabHora$HORA_REF[i]) & hour(tab$HORA_REF) == hour(tabHora$HORA_REF[i]))
    LATITUDE[i] = aux$LATITUDE[which(aux[,c(variavel)] == tabHora$x[i])[1]]
    LONGITUDE[i] = aux$LONGITUDE[which(aux[,c(variavel)] == tabHora$x[i])[1]]
    HORARIO[i] = aux$HORA_REF[which(aux[,c(variavel)] == tabHora$x[i])[1]]
  }
  
  tabHora = cbind(tabHora, LATITUDE, LONGITUDE, HORARIO)
  names(tabHora) = c("HORA_REF", variavel, "LATITUDE", "LONGITUDE", "HORARIO")
  tabHora = tabHora[,c("HORA_REF", "HORARIO", "LATITUDE", "LONGITUDE", variavel)]
  
  if(date(fim) == date(now())){
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), day(inicio), hour(now()) , 0, 0),
                          toData(year(fim), month(fim), day(fim), hour(now()) , 0, 0), "hour")
  }else{
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), day(inicio),  hour(now()) , 0, 0),
                          toData(year(fim), month(fim), day(fim), 23 , 0, 0), "hour")
  }
  
  tabHora = merge(tabHora, data.frame(HORA_REF), by = "HORA_REF", all.y = T)
  return(tabHora)
}

agregaConsumoMedidoDia = function(sensor, inicio = now()-days(30), fim = now(), metodo, variavel, tabelaDados = NULL){
  # inicio = date(inicio) 
  # fim = date(fim)
  
  if(is.null(tabelaDados)){
    tab = transfConsumoMedidoEvento(sensor, inicio, fim)
  }else{
    tab = subset(tabelaDados, tabelaDados$HORA_REF >= inicio & tabelaDados$HORA_REF <= fim)
  }
  tabDia = aggregate(tab[,c(variavel)], 
                      list(HORA_REF = toData(year(tab$HORA_REF), month(tab$HORA_REF), day(tab$HORA_REF), 1, 0, 0)), 
                      metodo)
  
  tabDia$HORA_REF= date(tabDia$HORA_REF)
  LATITUDE = c()
  LONGITUDE = c()
  HORARIO = rep(toData(0,0,0,0,0,0),nrow(tabDia))
  i=2
  for(i in 1:length(tabDia$HORA_REF)){
    aux = subset(tab, date(tab$HORA_REF) == date(tabDia$HORA_REF[i]))
    LATITUDE[i] = aux$LATITUDE[which(aux[,c(variavel)] == tabDia$x[i])[1]]
    LONGITUDE[i] = aux$LONGITUDE[which(aux[,c(variavel)] == tabDia$x[i])[1]]
    HORARIO[i] = aux$HORA_REF[which(aux[,c(variavel)] == tabDia$x[i])[1]]
      
  }
  
  tabDia = cbind(tabDia, LATITUDE, LONGITUDE, HORARIO)
  names(tabDia) = c("HORA_REF", variavel, "LATITUDE", "LONGITUDE", "HORARIO")
  tabDia = tabDia[,c("HORA_REF", "HORARIO", "LATITUDE", "LONGITUDE", variavel)]
  
  fim = fim + days(1)
  HORA_REF = date(seq.POSIXt(toData(year(inicio), month(inicio), day(inicio),  1 , 0, 0),
                             toData(year(fim), month(fim), day(fim), 1, 0, 0), "day"))
  
  tabDia = merge(tabDia, data.frame(HORA_REF), by = "HORA_REF", all.y = T)
  return(tabDia)
}

agregaConsumoMedidoMes = function(sensor, inicio = now()-month(6), fim = now(), metodo, variavel, tabelaDados = NULL){
  inicio = date(inicio) 
  fim = date(fim)
  
  if(is.null(tabelaDados)){
    tab = transfConsumoMedidoEvento(sensor, inicio, fim)
  }else{
    tab = subset(tabelaDados, tabelaDados$HORA_REF >= inicio & tabelaDados$HORA_REF <= fim)
  }
  tabMes = aggregate(tab[,c(variavel)], 
                     list(HORA_REF = toData(year(tab$HORA_REF), month(tab$HORA_REF), 1, 0, 0, 0)), 
                     metodo)
  LATITUDE = c()
  LONGITUDE = c()
  HORARIO = rep(toData(0,0,0,0,0,0),nrow(tabMes))
  i=2
  for(i in 1:length(tabMes$HORA_REF)){
    aux = subset(tab, year(tab$HORA_REF) == year(tabMes$HORA_REF[i]) & month(tab$HORA_REF) == month(tabMes$HORA_REF[i]))
    LATITUDE[i] = aux$LATITUDE[which(aux[,c(variavel)] == tabMes$x[i])[1]]
    LONGITUDE[i] = aux$LONGITUDE[which(aux[,c(variavel)] == tabMes$x[i])[1]]
    HORARIO[i] = aux$HORA_REF[which(aux[,c(variavel)] == tabMes$x[i])[1]]
    
  }
  
  tabMes = cbind(tabMes, LATITUDE, LONGITUDE, HORARIO)
  names(tabMes) = c("HORA_REF", variavel, "LATITUDE", "LONGITUDE", "HORARIO")
  tabMes = tabMes[,c("HORA_REF", "HORARIO", "LATITUDE", "LONGITUDE", variavel)]
  
  if(fim == date(now())){
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), 1, 0 , 0, 0),
                          toData(year(fim), month(fim), 1, 0 , 0, 0), "day")
  }else{
    HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), 1, 0 , 0, 0),
                          toData(year(fim), month(fim), 1, 0 , 0, 0), "day")
  }
  
  tabMes = merge(tabMes, data.frame(HORA_REF), by = "HORA_REF", all.y = T)
  return(tabMes)
}
