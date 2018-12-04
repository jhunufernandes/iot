pontos = carregaPontos()
sensoresAtual = function(tabela){
  SENSORES = subset(pontos$ID, pontos$SENSOR==1)
  tab = subset(tabela, tabela$LOCAL %in% SENSORES & (tabela$STATUS == 0 | tabela$STATUS == 1 | tabela$STATUS == 129 | tabela$STATUS == 128) )
  ULT_RESG = aggregate(tab$MOMENTO, list(tab$LOCAL), max2)$x
  FUNC_ATUAL = ifelse((now()- ULT_RESG)>0.125, 0, 1)

  return(data.frame(SENSORES, ULT_RESG, FUNC_ATUAL))
}
# sensoresAtual(tabela)
# head(tabela)

# sensoresAtual = function(tabela){
#   SENSORES = subset(pontos$ID, pontos$SENSOR==1)
#   tab = subset(tabela, tabela$LOCA %in% SENSORES & (tabela$STATUS == 0 | tabela$STATUS == 1 | tabela$STATUS == 129 | tabela$STATUS == 128) )
#   ULT_RESG = aggregate(tab$MOMENTO, list(tab$LOCAL), max2)$x
#   FUNC_ATUAL = ifelse((now()- ULT_RESG)>0.125, 0, 1)
#   
#   return(data.frame(SENSORES, ULT_RESG, FUNC_ATUAL))
# }
# 

# estatisticaEvento = function(tabela){
#   LOCAL
#   VOL_TOT
#   QTD_EVENTO
#   MED_EVENTO_DIA
#   MED_EVENTO_DIA
#   MED_EVENTO_DIA
#   SENSORES = subset(pontos$ID, pontos$SENSOR==1)
#   tab = subset(tabela, tabela$LOCA %in% SENSORES & (tabela$STATUS == 0 | tabela$STATUS == 1 | tabela$STATUS == 129 | tabela$STATUS == 128) )
#   ULT_RESG = aggregate(tab$MOMENTO, list(tab$LOCAL), max2)$x
#   FUNC_ATUAL = ifelse((now()- ULT_RESG)>0.125, 0, 1)
# 
#   return(data.frame(SENSORES, ULT_RESG, FUNC_ATUAL))
# }

estatisticaHoraDiaPeriodo = function(tabela){
  # tabelaS0 = subset(tabela, tabela$VOLUME>0)
  loc1 = list(tabela$LOCAL)
  # loc2 = list(tabelaS0$LOCAL)
  

  tabEstat = data.frame(
    LOCAL = aggregate(tabela$VOLUME, loc1, sum2)$Group.1,
    VOLUME_TOT = aggregate(tabela$VOLUME, loc1, sum2)$x,
    VOLUME_QDT = aggregate(tabela$VOLUME, loc1, length2)$x,
    VOLUME_MEDIO = aggregate(tabela$VOLUME, loc1, mean2)$x,
    VOLUME_VARIANCIA = aggregate(tabela$VOLUME, loc1, var2)$x,
    VOLUME_DESVIO = aggregate(tabela$VOLUME, loc1, sd2)$x,
    VOLUME_MIN = aggregate(tabela$VOLUME, loc1, min2)$x,
    VOLUME_Q1 = aggregate(tabela$VOLUME, loc1, q1_2)$x,
    VOLUME_MEDIANO = aggregate(tabela$VOLUME, loc1, median2)$x,
    VOLUME_Q3 = aggregate(tabela$VOLUME, loc1, q3_2)$x,
    VOLUME_MAX = aggregate(tabela$VOLUME, loc1, max2)$x,
    # VOLUME_HORA_MIN,
    # VOLUME_HORA_Q1,
    # VOLUME_HORA_MEDIANO,
    # VOLUME_HORA_Q3,
    # VOLUME_HORA_MAX,
    # VOLUME_QTD_OUTLIER,
    EVENTO_QTD_TOT = aggregate(tabela$QUANTIDADE, loc1, sum2)$x,
    EVENTO_QTD_MEDIA = aggregate(tabela$QUANTIDADE, loc1, mean2)$x,
    EVENTO_QTD_MAX = aggregate(tabela$QUANTIDADE, loc1, max2)$x,
    EVENTO_QTD_MIN = aggregate(tabela$QUANTIDADE, loc1, min2)$x
    
    # VOLUME_TOT_S0 = aggregate(tabelaS0$VOLUME, loc2, sum2)$x,
    # VOLUME_QDT_S0 = aggregate(tabelaS0$VOLUME, loc2, length2)$x,
    # VOLUME_MEDIO_S0 = aggregate(tabelaS0$VOLUME, loc2, mean2)$x,
    # VOLUME_VARIANCIA_S0 = aggregate(tabelaS0$VOLUME, loc2, var2)$x,
    # VOLUME_DESVIO_S0 = aggregate(tabelaS0$VOLUME, loc2, sd2)$x,
    # VOLUME_MIN_S0 = aggregate(tabelaS0$VOLUME, loc2, min2)$x,
    # VOLUME_Q1_S0 = aggregate(tabelaS0$VOLUME, loc2, q1_2)$x,
    # VOLUME_MEDIANO_S0 = aggregate(tabelaS0$VOLUME, loc2, median2)$x,
    # VOLUME_Q3_S0 = aggregate(tabelaS0$VOLUME, loc2, q3_2)$x,
    # VOLUME_MAX_S0 = aggregate(tabelaS0$VOLUME, loc2, max2)$x,
    # EVENTO_QTD_TOT_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, sum2)$x,
    # EVENTO_QTD_MEDIA_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, mean2)$x,
    # EVENTO_QTD_MAX_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, max2)$x,
    # EVENTO_QTD_MIN_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, min2)$x
  )
    # EVENTO_HORA_MAX,
    # EVENTO_HORA_MIN)
}


estatisticaEvento = function(tabela){
  # tabelaS0 = subset(tabela, tabela$VOLUME>0)
  loc1 = list(tabela$LOCAL)
  # loc2 = list(tabelaS0$LOCAL)
  
  
  tabEstat = data.frame(
    LOCAL = aggregate(tabela$VOLUME, loc1, sum2)$Group.1,
    VOLUME_TOT = aggregate(tabela$VOLUME, loc1, sum2)$x,
    VOLUME_QDT = aggregate(tabela$VOLUME, loc1, length2)$x,
    VOLUME_MEDIO = aggregate(tabela$VOLUME, loc1, mean2)$x,
    VOLUME_VARIANCIA = aggregate(tabela$VOLUME, loc1, var2)$x,
    VOLUME_DESVIO = aggregate(tabela$VOLUME, loc1, sd2)$x,
    VOLUME_MIN = aggregate(tabela$VOLUME, loc1, min2)$x,
    VOLUME_Q1 = aggregate(tabela$VOLUME, loc1, q1_2)$x,
    VOLUME_MEDIANO = aggregate(tabela$VOLUME, loc1, median2)$x,
    VOLUME_Q3 = aggregate(tabela$VOLUME, loc1, q3_2)$x,
    VOLUME_MAX = aggregate(tabela$VOLUME, loc1, max2)$x
    # VOLUME_HORA_MIN,
    # VOLUME_HORA_Q1,
    # VOLUME_HORA_MEDIANO,
    # VOLUME_HORA_Q3,
    # VOLUME_HORA_MAX,
    # VOLUME_QTD_OUTLIER,
    # EVENTO_QTD_TOT = aggregate(tabela$QUANTIDADE, loc1, sum2)$x,
    # EVENTO_QTD_MEDIA = aggregate(tabela$QUANTIDADE, loc1, mean2)$x,
    # EVENTO_QTD_MAX = aggregate(tabela$QUANTIDADE, loc1, max2)$x,
    # EVENTO_QTD_MIN = aggregate(tabela$QUANTIDADE, loc1, min2)$x
    # 
    # VOLUME_TOT_S0 = aggregate(tabelaS0$VOLUME, loc2, sum2)$x,
    # VOLUME_QDT_S0 = aggregate(tabelaS0$VOLUME, loc2, length2)$x,
    # VOLUME_MEDIO_S0 = aggregate(tabelaS0$VOLUME, loc2, mean2)$x,
    # VOLUME_VARIANCIA_S0 = aggregate(tabelaS0$VOLUME, loc2, var2)$x,
    # VOLUME_DESVIO_S0 = aggregate(tabelaS0$VOLUME, loc2, sd2)$x,
    # VOLUME_MIN_S0 = aggregate(tabelaS0$VOLUME, loc2, min2)$x,
    # VOLUME_Q1_S0 = aggregate(tabelaS0$VOLUME, loc2, q1_2)$x,
    # VOLUME_MEDIANO_S0 = aggregate(tabelaS0$VOLUME, loc2, median2)$x,
    # VOLUME_Q3_S0 = aggregate(tabelaS0$VOLUME, loc2, q3_2)$x,
    # VOLUME_MAX_S0 = aggregate(tabelaS0$VOLUME, loc2, max2)$x,
    # EVENTO_QTD_TOT_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, sum2)$x,
    # EVENTO_QTD_MEDIA_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, mean2)$x,
    # EVENTO_QTD_MAX_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, max2)$x,
    # EVENTO_QTD_MIN_S0 = aggregate(tabelaS0$QUANTIDADE, loc2, min2)$x
  )
  # EVENTO_HORA_MAX,
  # EVENTO_HORA_MIN)
}


# funcionamento = sensoresAtual(tabela)
# tabela = transfConsumoMedido_Ev(carregaConsumo(carregaPontos()))
# 
# plot(tabela$VOLUME)
