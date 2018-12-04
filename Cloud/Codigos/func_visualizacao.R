plotSeriesPlotly = function(x, y, xlab, ylab, main = "", dec = 3){
  dados = data.frame(x,y = round(y,dec))
  p = plot_ly(dados, x = ~x) %>%
    add_lines(y = ~y, name = " ") %>%
    layout(
      title = main,
      xaxis = list(title = xlab,
        rangeselector = list(
          buttons = list(
            list(
              count = 12,
              label = "12 Horas",
              step = "hour",
              stepmode = "backward"),
            list(
              count = 24,
              label = "Um dia",
              step = "hour",
              stepmode = "backward"),
            list(
              count = 24*7,
              label = "Uma semana",
              step = "hour",
              stepmode = "backward"),
            list(
              count = 24*30,
              label = "Um mês",
              step = "hour",
              stepmode = "backward"),
            
            list(step = "all"))),
        
        rangeslider = list(type = "date")),
      
      yaxis = list(title = ylab ))
  return(p)
}

plotPizzaPlotly = function(valores, labels, rend = 0){
  dados = data.frame(labels, valores = round(valores, rend))
  p <- plot_ly(dados, labels = ~labels, values = ~valores, type = 'pie',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               marker = list(line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE) %>%
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
}

plotBarraPlotly = function(valores, barras, ylab, xlab, main = "", rend = 0){
  datAux = data.frame(valores = round(valores, rend), barras)
  plot_ly(datAux, y = ~valores, x =~barras, type = "bar")%>%
    layout(title = main,
           xaxis = list(title = xlab),
           yaxis = list(title = ylab))
}

plotDisperPlotly = function(xval, yval, xlab, ylab,  main = "", rend = 0){
  datAux = data.frame(xval = round(xval, rend), yval = round(yval, rend))
  plot_ly(datAux, y = ~yval, x =~xval)%>%
    layout(title = main,
           xaxis = list(title = xlab),
           yaxis = list(title = ylab))
}


plotBoxPlotly = function(x, y, xlab, ylab, main = ""){
  datAux = data.frame(x, y)
  plot_ly(datAux, y = ~y, x =~x, type = "box") %>%
    layout(title = main,
           xaxis = list(title = xlab),
           yaxis = list(title = ylab))
}

plotHistPlotly = function(x, xlab, main = ""){
  plot_ly(x = x, type = "histogram") %>%
    layout(title = main,
           xaxis = list(title = xlab),
           yaxis = list(title = "Frequência"))
  
}

# plotHistPlotly(rnorm(1000),"lab", main = "hhhh")

# tabela = transfConsumoMedidoHora(transfConsumoMedido_Ev(carregaConsumo(carregaPontos())), inicio,fim)
# vasos = subset(tabela, tabela$LOCAL == "pia4")
# plotSeries(x = vasos$HORA_REF, y = vasos$VOLUME, xlab = "Tempo (Hora)", ylab = "Volume (litros)", main = "")
# tab = aggregate(tabela$VOLUME, list(tabela$LOCAL), sum2)
# tab = tab[1:7,]
# subset(tabela$VOLUME, is.na(tabela$VOLUME))
# plotPizzaPlotly(tab$x, tab$Group.1, 0)

