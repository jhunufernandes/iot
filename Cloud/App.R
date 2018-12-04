setwd("/home/laftos/Projetos/MedidorPoluicao")

source("Codigos/pacotes.R")
source("Codigos/func_auxiliar.R")
source("Codigos/func_corrigeDados.R")
source("Codigos/func_visualizacao.R")
source("Codigos/func_interface.R")

geraDados1 = function(){
  N = 6*24
  v1 = rnorm(N, 0.1)/1000
  v2 = rnorm(N, 0.1)/1000
  v3 = rnorm(N, 1.8)
  v4 = rnorm(N)
  hora = 0:23
  min = 0:59
  seg = 0:59
  #horario = expand.grid(seg, min, hora)
  horario = expand.grid(min, hora)
  lat = c(-23.56076779)
  lon = c(-46.72976164)
  val1 = c(30)
  val2 = c(25)
  for (i in 2:N) {
    lat[i] = lat[i-1]+v1[i]
    lon[i] = lon[i-1]+v2[i]
    val1[i] = val1[i-1]+v3[i]
    val2[i] = val2[i-1]+v4[i]
  }
  inter = (max(val1) - min(val1))/5
  m = min(val1)
  quali1 = classificaPoluicao(val1)
  quali2 = classificaTemperatura(val2)
  tabela = data.frame(HORA = horario$Var2,
                      MINUTO = horario$Var1,
                      SEGUNDO = rep(0, N),#$horario$Var1,
                      LATITUDE = lat,
                      LONGITUDE = lon,
                      POLUICAO = round(val1,2),
                      QUALI_POLUICAO = quali1,
                      TEMPERATURA = round(val2,2),
                      QUALI_TEMPERATURA = quali2)
  return(tabela)
}
geraDados2 = function(inicio, fim){
  HORA_REF = seq.POSIXt(toData(year(inicio), month(inicio), day(inicio), hour(inicio) , minute(inicio), 0),
                        toData(year(fim), month(fim), day(fim), hour(fim) , minute(fim), 0), "min")
  N = length(HORA_REF)
  v1 = rnorm(N, 0)/1000
  v2 = rnorm(N, 0)/1000
  v3 = rnorm(N, 0, 3)
  v4 = rnorm(N, 0, 2)
  
  #horario = expand.grid(seg, min, hora)
  # horario = expand.grid(min, hora)
  lat = c(-23.56076779)
  lon = c(-46.72976164)
  val1 = c(30)
  val2 = c(25)
  for (i in 2:N) {
    lat[i] = lat[i-1]+v1[i]
    lon[i] = lon[i-1]+v2[i]
    val1[i] = val1[i-1]+v3[i]
    val1[i] = ifelse(val1[i]<(0), 0,  
                   ifelse(val1[i]>(300),300, val1[i]))
    val2[i] = val2[i-1]+v4[i]
    val2[i] = ifelse(val2[i]<(-10), -10,  
                     ifelse(val2[i]>(45),45, val2[i]))
  }
  
  # quali1 = classificaPoluicao(val1)
  # quali2 = classificaTemperatura(val2)
  # HORA_REF = toData()
  tabela = data.frame(HORA_REF,
                      LATITUDE = lat,
                      LONGITUDE = lon,
                      POLUICAO = round(val1,2),
                      #QUALI_POLUICAO = quali1,
                      TEMPERATURA = round(val2,2))
                      #QUALI_TEMPERATURA = quali2)
  return(tabela)
}

##########################################
### USER INTERFACE - PROGRAMA PRINCIPAL #######################################################################################
##########################################

poluicao_carregaHistorico = function(input, output, sensor, tab = NULL){
  func = ifelse(input$radAgrega == "agrMax", max3,
                ifelse(input$radAgrega == "agr3Q", q3_3,
                       ifelse(input$radAgrega == "agrMed", median3,
                              ifelse(input$radAgrega == "agr1Q", q1_3, 
                                     ifelse(input$radAgrega == "agrMin", min3, NA)))))

  if(input$radPeriodo == "ultDia"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-days(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-days(1)), fim = now(),
                                            metodo = func, variavel = "POLUICAO", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else if (as.character(input$radPeriodo) == "ultSemana"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-weeks(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-weeks(1)), fim = now(),
                                            metodo = func, variavel = "POLUICAO", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else if (as.character(input$radPeriodo) == "ultMes"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-months(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-months(1)), fim = now(),
                                            metodo = func, variavel = "POLUICAO", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else{
    return("Erro!")
  }
  
  tabelaLocal = data.frame(tabelaLocal,
                           QUALI_POLUICAO = classificaPoluicao(tabelaLocal$POLUICAO))
  
  tab_FreqQualiAr = aggregate(tabelaLocal[,"QUALI_POLUICAO"], list(QUALI_POLUICAO= tabelaLocal$QUALI_POLUICAO), length2)
  names(tab_FreqQualiAr) = c("QUALIDADE","FREQUENCIA")
  
  output$box_poluicaoMax <- renderInfoBox({
    infoBox("Poluição máxima registrada:",
            paste(formatar(max2(tabelaLocal$POLUICAO)), "ppm"),
            icon = icon("square"),
            color = corPoluicao(classificaPoluicao(max2(tabelaLocal$POLUICAO))),
            fill = TRUE)
  })
  output$box_poluicaoMedia <- renderInfoBox({
    infoBox("Poluição média registrada:",
            paste(formatar(mean2(tabelaLocal$POLUICAO)), "ppm"),
            icon = icon("square"),
            color = corPoluicao(classificaPoluicao(mean2(tabelaLocal$POLUICAO))),
            fill = TRUE)
  })
  output$box_poluicaoMin <- renderInfoBox({
    infoBox("Poluição mínima registrada:",
            paste(formatar(min2(tabelaLocal$POLUICAO)), "ppm"),
            icon = icon("square"),
            color = corPoluicao(classificaPoluicao(min2(tabelaLocal$POLUICAO))),
            fill = TRUE)
  })
  
  output$box_poluicaoQualiMaisFreq <- renderInfoBox({
    infoBox("Índice de poluição mais frequente:",
            tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == max(tab_FreqQualiAr$FREQUENCIA))[1]],
            icon = icon("square"),
            color = corPoluicao(tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == max(tab_FreqQualiAr$FREQUENCIA))[1]]),
            fill = TRUE)
  })
  output$box_poluicaoQualiMenosFreq <- renderInfoBox({
    infoBox("Índice de poluição menos frequente:",
            tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == min(tab_FreqQualiAr$FREQUENCIA))[1]],
            icon = icon("square"),
            color = corPoluicao(tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == min(tab_FreqQualiAr$FREQUENCIA))[1]]),
            fill = TRUE)
  })
  output$box_poluicaoQdt <- renderInfoBox({
    infoBox("Quantidade de observações:",
            sum(tab_FreqQualiAr$FREQUENCIA),
            icon = icon("square"),
            color = "aqua",
            fill = TRUE)
  })

  output$map_histPoluicao = renderLeaflet({
    Color_Assets <- colorFactor(c("red", "orange", "yellow", "yellowgreen", "green"), 
                                levels =  c("Péssimo", "Ruim", "Regular", "Bom", "Ótimo"), ordered=FALSE)
    
    LABELS = paste0("<font color=\"black\"><b>DATA</b>: ", day(tabelaLocal$HORA_REF), ".", month(tabelaLocal$HORA_REF), ".", year(tabelaLocal$HORA_REF), "</br>",
                    "<b>HORA</b>: ", str_pad(hour(tabelaLocal$HORA_REF), 2, pad = 0), ":", str_pad(minute(tabelaLocal$HORA_REF), 2, pad = 0), "</br>",
                    "<b>POLUIÇÃO</b>: ", formatar2(tabelaLocal$POLUICAO, 3), " ppm","</br>",
                    "<b>QUALIDADE</b>: <font color=\"", Color_Assets(tabelaLocal$QUALI_POLUICAO),"\">", tabelaLocal$QUALI_POLUICAO, "</font></br></font>")
    
    map = leaflet(data = tabelaLocal) %>%
      clearBounds() %>% addTiles()  %>%
      addCircles(~LONGITUDE, ~LATITUDE,  color = ~Color_Assets(tabelaLocal$QUALI_POLUICAO),  
                 popup = ~LABELS, weight = 8,
                 popupOptions = popupOptions()) %>%
      addLegend("bottomright", pal = Color_Assets, values = tabelaLocal$QUALI_POLUICAO, title = "Qualidade do Ar")

    map <- map %>% addProviderTiles(providers$OpenStreetMap, group = "Mapa de rua 1" )
    map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap, group = "Mapa de rua 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Mapa cinza 1")
    map <- map %>% addProviderTiles(providers$Esri.WorldTopoMap, group = "Mapa cinza 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldPhysical, group = "Mapa físico")
    map <- map %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satélite")
    
    tipMap = c("Mapa de rua 1", "Mapa de rua 2","Mapa cinza 1","Mapa cinza 2", "Mapa físico", "Satélite")

    map %>%
      addLayersControl(baseGroups = tipMap,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(tiles = providers$OpenStreetMap, toggleDisplay = TRUE,
                 position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  })
  
#   ## GRÁFICOS ----------------------------------------------------------------------------------------------
  output$plot_histPoluicao = renderPlotly({
    plotSeriesPlotly(tabelaLocal$HORA_REF, tabelaLocal$POLUICAO, paste0("Tempo (", tempo, ")"), "Poluição (ppm)")
  })

}

temperatura_carregaHistorico = function(input, output, sensor, tab = NULL){
  func = ifelse(input$radAgrega == "agrMax", max3,
                ifelse(input$radAgrega == "agr3Q", q3_3,
                       ifelse(input$radAgrega == "agrMed", median3,
                              ifelse(input$radAgrega == "agr1Q", q1_3, 
                                     ifelse(input$radAgrega == "agrMin", min3, NA)))))
  
  if(input$radPeriodo == "ultDia"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-days(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-days(1)), fim = now(),
                                            metodo = func, variavel = "TEMPERATURA", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else if (as.character(input$radPeriodo) == "ultSemana"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-weeks(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-weeks(1)), fim = now(),
                                            metodo = func, variavel = "TEMPERATURA", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else if (as.character(input$radPeriodo) == "ultMes"){
    if(is.na(func)){
      tabelaLocal = subset(tab, tab$HORA_REF>= (now()-months(1)))
      tempo = "Minutos"
      
    }else{
      tabelaLocal = agregaConsumoMedidoHora(sensor, inicio = (now()-months(1)), fim = now(),
                                            metodo = func, variavel = "TEMPERATURA", tabelaDados = tab)
      tempo = "Horas"
    }
  }
  else{
    return("Erro!")
  }
  
  
  tabelaLocal = data.frame(tabelaLocal,
                           QUALI_TEMPERATURA = classificaTemperatura(tabelaLocal$TEMPERATURA))
  
  tab_FreqQualiAr = aggregate(tabelaLocal[,"QUALI_TEMPERATURA"], list(QUALI_TEMPERATURA= tabelaLocal$QUALI_TEMPERATURA), length2)
  names(tab_FreqQualiAr) = c("QUALIDADE","FREQUENCIA")
  
  output$box_temperaturaMax <- renderInfoBox({
    infoBox("Temperatura máxima registrada:",
            paste(formatar(max2(tabelaLocal$TEMPERATURA)), "°C"),
            icon = icon("square"),
            color = corTemperatura(classificaTemperatura(max2(tabelaLocal$TEMPERATURA))),
            fill = TRUE)
  })
  output$box_temperaturaMedia <- renderInfoBox({
    infoBox("Temperatura média registrada:",
            paste(formatar(mean2(tabelaLocal$TEMPERATURA)), "°C"),
            icon = icon("square"),
            color = corTemperatura(classificaTemperatura(mean2(tabelaLocal$TEMPERATURA))),
            fill = TRUE)
  })
  output$box_temperaturaMin <- renderInfoBox({
    infoBox("Temperatura mínima registrada:",
            paste(formatar(min2(tabelaLocal$TEMPERATURA)), "°C"),
            icon = icon("square"),
            color = corTemperatura(classificaTemperatura(min2(tabelaLocal$TEMPERATURA))),
            fill = TRUE)
  })
  
  output$box_temperaturaQualiMaisFreq <- renderInfoBox({
    infoBox("Temperaturas mais frequentes:",
            tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == max(tab_FreqQualiAr$FREQUENCIA))[1]],
            icon = icon("square"),
            color = corTemperatura(tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == max(tab_FreqQualiAr$FREQUENCIA))[1]]),
            fill = TRUE)
  })
  output$box_temperaturaQualiMenosFreq <- renderInfoBox({
    infoBox("Temperaturas menos frequentes:",
            tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == min(tab_FreqQualiAr$FREQUENCIA))[1]],
            icon = icon("square"),
            color = corTemperatura(tab_FreqQualiAr$QUALIDADE[which(tab_FreqQualiAr$FREQUENCIA == min(tab_FreqQualiAr$FREQUENCIA))[1]]),
            fill = TRUE)
  })
  output$box_temperaturaQdt <- renderInfoBox({
    infoBox("Quantidade de observações:",
            sum(tab_FreqQualiAr$FREQUENCIA),
            icon = icon("square"),
            color = "aqua",
            fill = TRUE)
  })
  
  
  output$map_histTemperatura = renderLeaflet({
    Color_Assets <- colorFactor(c("red", "orange", "yellow", "lightblue", "blue"), 
                                levels =  c("Muito Quente", "Quente", "Agradável", "Frio", "Muito Frio"), ordered=FALSE)
    
    LABELS = paste0("<font color=\"black\"><b>DATA</b>: ", day(tabelaLocal$HORA_REF), ".", month(tabelaLocal$HORA_REF), ".", year(tabelaLocal$HORA_REF), "</br>",
                    "<b>HORA</b>: ", str_pad(hour(tabelaLocal$HORA_REF), 2, pad = 0), ":", str_pad(minute(tabelaLocal$HORA_REF), 2, pad = 0), "</br>",
                    "<b>TEMPERATURA</b>: ", formatar2(tabelaLocal$TEMPERATURA, 3), " °C","</br>",
                    "<b>QUALIDADE</b>: <font color=\"", Color_Assets(tabelaLocal$QUALI_TEMPERATURA),"\">", tabelaLocal$QUALI_TEMPERATURA, "</font></br></font>")
    
    map = leaflet(data = tabelaLocal) %>%
      clearBounds() %>% addTiles()  %>%
      addCircles(~LONGITUDE, ~LATITUDE,  color = ~Color_Assets(tabelaLocal$QUALI_TEMPERATURA),  
                 popup = ~LABELS, weight = 8,
                 popupOptions = popupOptions()) %>%
      addLegend("bottomright", pal = Color_Assets, values = tabelaLocal$QUALI_TEMPERATURA, title = "Temperatura")
    
    map <- map %>% addProviderTiles(providers$OpenStreetMap, group = "Mapa de rua 1" )
    map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap, group = "Mapa de rua 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Mapa cinza 1")
    map <- map %>% addProviderTiles(providers$Esri.WorldTopoMap, group = "Mapa cinza 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldPhysical, group = "Mapa físico")
    map <- map %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satélite")
    
    tipMap = c("Mapa de rua 1", "Mapa de rua 2","Mapa cinza 1","Mapa cinza 2", "Mapa físico", "Satélite")
    
    map %>%
      addLayersControl(baseGroups = tipMap,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(tiles = providers$OpenStreetMap, toggleDisplay = TRUE,
                 position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
  })
  
  ### GRÁFICOS ----------------------------------------------------------------------------------------------
  output$plot_histTemperatura = renderPlotly({
    plotSeriesPlotly(tabelaLocal$HORA_REF, tabelaLocal$TEMPERATURA, paste0("Tempo (", tempo, ")"), "Temperatura (°C)")
  })
  
}

Geral_carregaTempoReal = function(input, output, sensor){
  
  hoje =  ymd_hms(now(), tz="America/Sao_Paulo")
  arq = paste0("Dados/Desagregado/Sensores/", sensor, "/", str_pad(day(hoje),2 ,pad = 0), str_pad(month(hoje),2 ,pad = 0), year(hoje), ".csv")
  
  while (!file.exists(arq)) {
    hoje = hoje - days(1)
    arq = paste0("Dados/Desagregado/Sensores/", sensor, "/", str_pad(day(hoje),2 ,pad = 0), str_pad(month(hoje),2 ,pad = 0), year(hoje), ".csv")
  }
  
  dadosAtual = read.table(arq, header=FALSE, skip=countLines(arq)-1, sep = ";", dec = ',')
  
  #dadosAtual = read.table(arq, header=FALSE,  sep = ";", dec = ',')
  names(dadosAtual) = c("HORA", "MINUTO","SEGUNDO", "LATITUDE", "LONGITUDE", "POLUICAO", "TEMPERATURA")
  
  dadosAtual = data.frame(dadosAtual,
                          HORA_REF = toData(year(hoje), month(hoje), day(hoje), dadosAtual$HORA, dadosAtual$MINUTO, dadosAtual$SEGUNDO),
                          QUALI_POLUICAO = classificaPoluicao(dadosAtual$POLUICAO),
                          QUALI_TEMPERATURA = classificaTemperatura(dadosAtual$TEMPERATURA))
   
  
  # torneirasEstats = estatisticaHoraDiaPeriodo(torneirasTab2)
  # tab_FreqQualiAr  = aggregate(tabela$QUALI_POLUICAO, list(tabela$QUALI_POLUICAO), length2)
  # names(tab_FreqQualiAr) = c("QUALIDADE", "FREQUENCIA")
  
  ## VALUEBOX -----------------------------------------------------------------------------------------------------
  strHora = paste0(str_pad(dadosAtual$HORA[1], 2, pad = 0), ":", 
                   str_pad(dadosAtual$MINUTO[1], 2, pad = 0), ":", 
                   str_pad(round(dadosAtual$SEGUNDO[1]), 2, pad = 0))
  
  output$box_ultimaAtualizacao <- renderInfoBox({
    valueBox(strHora,
            paste0(format.Date(hoje, "%d/%m/%Y")),
            icon = NULL,
            color = "aqua")
  })
  output$box_poluicaoAtual <- renderInfoBox({
    polu = paste0(round(dadosAtual$POLUICAO[1]), " ppm - ", dadosAtual$QUALI_POLUICAO[1],"")
    valueBox(polu,
             "QUALIDADE DO AR",
             icon = icon("cloud", lib = "glyphicon"),
             color = corPoluicao(dadosAtual$QUALI_POLUICAO[1]))
  })
  output$box_temperaturaAtual <- renderInfoBox({
    temp = paste0(round(dadosAtual$TEMPERATURA[1]), " °C - ", dadosAtual$QUALI_TEMPERATURA[1],"")
    valueBox(temp,
             "TEMPERATURA",
             icon = icon("fire", lib = "glyphicon"),
             color = corTemperatura(dadosAtual$QUALI_TEMPERATURA[1]))
  })
  # tabela = data.frame(LONGITUDE = dadosAtual$LONGITUDE,
  #                     LATITUDE = dadosAtual$LATITUDE,
  #                     DATA = paste0(format.Date(hoje, "%d/%m/%Y"), " ", strHora),
  #                     POLUICAO = dadosAtual$QUALI_POLUICAO,
  #                     TEMPERATURA = dadosAtual$QUALI_TEMPERATURA) 

  # coordinates(tabela) <- ~ LONGITUDE + LATITUDE
  # proj4string(tabela) <- "+init=epsg:4326"

  output$map_atual = renderLeaflet({
    Color_Assets <- colorFactor(c("red", "orange", "yellow", "yellowgreen", "green"), 
                                levels =  c("Péssimo", "Ruim", "Regular", "Bom", "Ótimo"), ordered=FALSE)
    Color_AssetsTemp <- colorFactor(c("red", "orange", "yellow", "lightblue", "blue"), 
                                levels =  c("Muito Quente", "Quente", "Agradável", "Frio", "Muito Frio"), ordered=FALSE)
    
    
    LABELS = paste0("<font color=\"black\"><b>DATA</b>: ", day(dadosAtual$HORA_REF), ".", month(dadosAtual$HORA_REF), ".", year(dadosAtual$HORA_REF), "</br>",
                    "<b>HORA</b>: ", str_pad(hour(dadosAtual$HORA_REF), 2, pad = 0), ":", str_pad(minute(dadosAtual$HORA_REF), 2, pad = 0), "</br>",
                    "<b>POLUIÇÃO</b>: ", formatar2(dadosAtual$POLUICAO, 3), " ppm","</br>",
                    "<b>QUALIDADE DO AR</b>: <font color=\"", Color_Assets(dadosAtual$QUALI_POLUICAO),"\">", dadosAtual$QUALI_POLUICAO, "</font></br>",
                    "<b>TEMPERATURA</b>: ", formatar2(dadosAtual$TEMPERATURA, 3), " °C","</br>",
                    "<b>NÍVEL TEMPERATURA</b>: <font color=\"", Color_AssetsTemp(dadosAtual$QUALI_TEMPERATURA),"\">", dadosAtual$QUALI_TEMPERATURA, "</font></br>","</font>")
    
    map = leaflet(data = dadosAtual) %>%
      clearBounds() %>% addTiles()  %>%
      addCircles(~LONGITUDE, ~LATITUDE,  color = 'black',  
                 popup = ~LABELS, weight = 8,
                 popupOptions = popupOptions()) #%>%
      # addLegend("bottomright", pal = Color_Assets, values = dadosAtual$QUALI_POLUICAO, title = "Qualidade do Ar")
    
    map <- map %>% addProviderTiles(providers$OpenStreetMap, group = "Mapa de rua 1" )
    map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap, group = "Mapa de rua 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Mapa cinza 1")
    map <- map %>% addProviderTiles(providers$Esri.WorldTopoMap, group = "Mapa cinza 2")
    map <- map %>% addProviderTiles(providers$Esri.WorldPhysical, group = "Mapa físico")
    map <- map %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satélite")
    
    
    
    tipMap = c("Mapa de rua 1", "Mapa de rua 2","Mapa cinza 1","Mapa cinza 2", "Mapa físico", "Satélite")
    
    map %>%
      addLayersControl(baseGroups = tipMap,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(tiles = providers$OpenStreetMap, toggleDisplay = TRUE,
                 position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
    # mapview(tabela, zcol = "POLUICAO",  at = c("Ótima", "Bom", "Regular", "Ruim", "Péssimo"), burst = TRUE, legend=T)
  })
}

server = function(input, output, session){
  info = read.table("/home/laftos/Projetos/MedidorPoluicao/Dados/Configuracao/listaSensor.csv", sep = ";", dec = ".", header = T)
  # acesso = FALSE
  # while (acesso == FALSE) {
  #   observe({
  #     cont = 0
  #     if(cont==0){
  #       showModal(modalDialog(
  #         title = "Atenção!",
  #         textInput("txt_sensorID", "Digite o código do seu produto:"),
  #         passwordInput("txt_sensorSenha", "Digite senha:"),
  #         
  #         easyClose = TRUE,
  #         footer = tagList(
  #           actionButton("ok", "OK")
  #         )
  #       ))
  #       sensor = as.character(input$txt_sensorID)
  #       senha = as.character(input$txt_sensorID)
  #     }else{
  #       showModal(modalDialog(
  #         title = "Atenção!",
  #         textInput("txt_sensorIDRep", "Digite o código do seu produto:"),
  #         passwordInput("txt_sensorSenhaRep", "Digite senha:"),
  #         "Código ou senha inválidos!",
  #         easyClose = TRUE,
  #         footer = tagList(
  #           actionButton("ok", "OK")
  #         )
  #       ))
  #       sensor = as.character(input$txt_sensorIDRep)
  #       senha = as.character(input$txt_sensorSenhaRep)
  #     }
  #     
  #     if(sensor %in% info$SENSOR){
  #       if(senha == info$SENHA[which(info$SENSOR==sensor)]){
  #         acesso = TRUE
  #       }
  #     }
  #   })
  #   
  # }
  sensor = "sensorAlea"
  observe({
    invalidateLater(20*1000, session)
    Geral_carregaTempoReal(input, output, sensor)
  })
  tab = geraDados2((now()-days(7)), (now()))
  observe({
    temperatura_carregaHistorico(input, output, sensor, tab)
  })
  observe({
    poluicao_carregaHistorico(input, output, sensor, tab)
  })
  observeEvent(input$btn_salvar,{
    info$EMAIL[which(info$SENSOR==sensor)] = as.character(input$txt_configEmail)
    info$SENHA[which(info$SENSOR==sensor)] = as.character(input$txt_configSenha)
    # info$PORTA[which(info$SENSOR==sensor)] = input$txt_configEmail
    info$ALARME[which(info$SENSOR==sensor)] = (input$chk_alarmes)
    info$EMAIL_DIARIO[which(info$SENSOR==sensor)] = (input$chk_emailDiario)
    info$EMAIL_SEMANAL[which(info$SENSOR==sensor)] = (input$chk_emailSemanal)
    info$EMAIL_MENSAL[which(info$SENSOR==sensor)] = (input$chk_emailMensal)

    write.csv2(info, "/home/laftos/Projetos/MedidorPoluicao/Dados/Configuracao/listaSensor.csv", row.names = F)
    
    showModal(modalDialog(
      title = "Sucesso!",
      "As informações foram salvas com sucesso!",
      easyClose = TRUE
    ))
    
  })
}

shinyApp(ui, server)