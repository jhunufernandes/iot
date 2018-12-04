carregaSensores = function(){
  return(read.table("Dados/Configuracao/listaSensor.csv", sep = ";", dec = ",", header = T))
}

carregaConsumo = function(pontos){
  datCons = data.frame()
  for (pto in pontos$PASTA){
    arquivos = paste0("Dados/Banheiro Masculino - Entrada Principal/", pto)
    for(arq in dir(arquivos)){
      tabAux =  read.table(paste0("Dados/Banheiro Masculino - Entrada Principal/", pto, "/", arq), sep = ";", dec = ",", header = F) #fread(paste0("Dados/Banheiro Masculino - Entrada Principal/", pto, "/", arq), sep = ";", dec = ",", header = F) #
      names(tabAux) = c("HORA", "MINUTO", "SEGUNDO", "STATUS", "INICIO", "FINAL", "DURACAO")
      ENDECONC = rep(as.integer(substr(arq, 1, 2)), nrow(tabAux))
      ENDEDISP = rep(as.integer(substr(arq, 3, 4)), nrow(tabAux))
      ANO = rep(as.integer(substr(arq, 5, 8)), nrow(tabAux))
      MES = rep(as.integer(substr(arq, 9, 10)), nrow(tabAux))
      DIA = rep(as.integer(substr(arq, 11, 12)), nrow(tabAux))
      LOCAL = rep(as.character(pontos$ID[which(pontos$PASTA == pto)]), nrow(tabAux))
      tabAux = (cbind(LOCAL, ENDECONC, ENDEDISP, ANO, MES, DIA, tabAux))
      datCons = rbind(datCons, tabAux)
    }
  }
  return(datCons)
}

carregaConsumo2 = function(pontos){
  listPontos = list()
  for (pto in pontos$PASTA){
    arquivos = paste0("Dados/Banheiro Masculino - Entrada Principal/", pto)
    for(arq in dir(arquivos)){
      tabAux = read.table(paste0("Dados/Banheiro Masculino - Entrada Principal/", pto, "/", arq), sep = ";", dec = ",", header = F)
      names(tabAux) = c("HORA", "MINUTO", "SEGUNDO", "STATUS", "INICIO", "FINAL", "DURACAO")
      ENDECONC = rep(as.integer(substr(arq, 1, 2)), nrow(tabAux))
      ENDEDISP = rep(as.integer(substr(arq, 3, 4)), nrow(tabAux))
      ANO = rep(as.integer(substr(arq, 5, 8)), nrow(tabAux))
      MES = rep(as.integer(substr(arq, 9, 10)), nrow(tabAux))
      DIA = rep(as.integer(substr(arq, 11, 12)), nrow(tabAux))
      tabAux = cbind(ENDECONC, ENDEDISP, ANO, MES, DIA, tabAux)
       
      listPontos[[as.character(pontos$ID[which(pontos$PASTA == pto)])]] = rbind(tabAux, listPontos[[as.character(pontos$ID[which(pontos$PASTA == pto)])]])
    }
  }
  return(listPontos)
}

# getwd()
# pontos = read.table("Dados/Banheiro Masculino - Entrada Principal/Outros/pontos.csv", sep = ";", dec = ".", header = T)
# consumo = carregaConsumo(pontos)
