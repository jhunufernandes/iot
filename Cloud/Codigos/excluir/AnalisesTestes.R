setwd("/home/laftos/Projetos/Monitoramento")

source("Codigos/pacotes.R")
source("Codigos/func_auxiliar.R")
source("Codigos/func_carregaDados.R")
source("Codigos/func_corrigeDados.R")
source("Codigos/func_estatisticas.R")
source("Codigos/func_visualizacao.R")
source("Codigos/func_interface.R")

#inicioMax = as.Date("2018-04-16")
#fimMax = as.Date("2018-06-16")


tabConsumo = carregaConsumo(pontos)
tabEv = transfConsumoMedido_Ev(tabConsumo)
tabHora = transfConsumoMedidoHora(tabEv, inicioMax, fimMax)
tabPeriodo = transfConsumoMedidoPeriodo(tabHora)
tabDia = transfConsumoMedidoDia(tabHora)
write.csv2(tabEv, "Dados/Analise/DadosCompleto_PorEvento.csv", row.names = F)
write.csv2(tabHora, "Dados/Analise/DadosCompleto_PorHora.csv", row.names = F)
write.csv2(tabPeriodo, "Dados/Analise/DadosCompleto_PorPeriodo.csv", row.names = F)
write.csv2(tabDia, "Dados/Analise/DadosCompleto_PorDia.csv", row.names = F)

par(mfrow = c(1,2))
test = subset(tabPeriodo, tabPeriodo$LOCAL=="torneira")
port = subset(tabPeriodo, tabPeriodo$LOCAL=="pess")


plot(test$QUANTIDADE~port$VOLUME, main = "ACIONAMENTOS")
plot(test$VOLUME~port$VOLUME, main = "VOLUME")

tabs = tabDia

DADOS = subset(tabs, tabs$LOCAL=="torneira")[c(2,3)]#[c(2,3,4,10)]
TORNEIRA_QTD = subset(tabs, tabs$LOCAL=="torneira")$QUANTIDADE
TORNEIRA_VOL = subset(tabs, tabs$LOCAL=="torneira")$VOLUME
MICTORIO_QTD = subset(tabs, tabs$LOCAL=="mictorio")$QUANTIDADE
MICTORIO_VOL = subset(tabs, tabs$LOCAL=="mictorio")$VOLUME
CONTADOR = subset(tabs, tabs$LOCAL=="pess")$VOLUME

PIA4_QTD = subset(tabs, tabs$LOCAL=="pia4")$QUANTIDADE
PIA4_VOL = subset(tabs, tabs$LOCAL=="pia4")$VOLUME
PIA5_QTD = subset(tabs, tabs$LOCAL=="pia5")$QUANTIDADE
PIA5_VOL = subset(tabs, tabs$LOCAL=="pia5")$VOLUME
PIA6_QTD = subset(tabs, tabs$LOCAL=="pia6")$QUANTIDADE
PIA6_VOL = subset(tabs, tabs$LOCAL=="pia6")$VOLUME

MIC1_QTD = subset(tabs, tabs$LOCAL=="mic1")$QUANTIDADE
MIC1_VOL = subset(tabs, tabs$LOCAL=="mic1")$VOLUME
MIC2_QTD = subset(tabs, tabs$LOCAL=="mic2")$QUANTIDADE
MIC2_VOL = subset(tabs, tabs$LOCAL=="mic2")$VOLUME
MIC3_QTD = subset(tabs, tabs$LOCAL=="mic3")$QUANTIDADE
MIC3_VOL = subset(tabs, tabs$LOCAL=="mic3")$VOLUME

QTD = data.frame(DADOS, MIC1_QTD, MIC2_QTD, MIC3_QTD, PIA4_QTD,PIA5_QTD,PIA6_QTD, TORNEIRA_QTD, MICTORIO_QTD, CONTADOR)
VOL = data.frame(DADOS, MIC1_VOL, MIC2_VOL, MIC3_VOL, PIA4_VOL,PIA5_VOL,PIA6_VOL, TORNEIRA_VOL, MICTORIO_VOL, CONTADOR)
write.csv2(QTD, "Dados/Analise/DadosUsadoNoAjuste_QTD.csv", row.names = F)
write.csv2(VOL, "Dados/Analise/DadosUsadoNoAjuste_VOL.csv", row.names = F)


round(cor(QTD[CONTADOR!=0,]), 2)
round(cor(VOL[CONTADOR!=0,]), 2)


##################################################
## MODELOS PARA VOLUME USANDO APENAS O CONTADOR ####################################################
##################################################

jpeg("Dados/Analise/modPia4_Vol.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$PIA4_VOL != 0),]
modPia4_Vol = lm((MOD_PIAS_VOL$PIA4_VOL) ~ MOD_PIAS_VOL$CONTADOR)
resum = summary(modPia4_Vol)
plot(MOD_PIAS_VOL$PIA4_VOL~MOD_PIAS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nTorneira 1 x Contador", ylab = "Consumo torneira 1 (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modPia4_Vol, lwd = 2, col = "red")
MOD_PIAS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modPia4_Vol, MOD_PIAS_VOL, interval = "prediction")
points(pd[,2]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$PIA4_VOL != 0),]
plot((modPia4_Vol$fitted.values)~MOD_PIAS_VOL$PIA4_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneira 1 ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_VOL$PIA4_VOL)
TAB = data.frame(OBSERVADO = MOD_PIAS_VOL$PIA4_VOL, PREDITO = modPia4_Vol$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modPia4_Vol.csv")
dev.off()


jpeg("Dados/Analise/modPia5_Vol.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$PIA5_VOL != 0),]
modPia5_Vol = lm((MOD_PIAS_VOL$PIA5_VOL) ~ MOD_PIAS_VOL$CONTADOR)
resum = summary(modPia5_Vol)
plot(MOD_PIAS_VOL$PIA5_VOL~MOD_PIAS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nTorneira 2 x Contador", ylab = "Consumo torneira 2 (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modPia5_Vol, lwd = 2, col = "red")
MOD_PIAS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modPia5_Vol, MOD_PIAS_VOL, interval = "prediction")
points(pd[,2]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$PIA5_VOL != 0),]
plot((modPia5_Vol$fitted.values)~MOD_PIAS_VOL$PIA5_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneira 2 ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_VOL$PIA5_VOL)
TAB = data.frame(OBSERVADO = MOD_PIAS_VOL$PIA5_VOL, PREDITO = modPia5_Vol$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modPia5_Vol.csv")
dev.off()


jpeg("Dados/Analise/modMic1_Vol.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC1_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
modMic1_Vol = lm((MOD_MICS_VOL$MIC1_VOL) ~ MOD_MICS_VOL$CONTADOR)
resum = summary(modMic1_Vol)
plot(MOD_MICS_VOL$MIC1_VOL~MOD_MICS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nMictório 1 x Contador", ylab = "Consumo mictório 1 (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic1_Vol, lwd = 2, col = "red")
MOD_MICS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modMic1_Vol, MOD_MICS_VOL, interval = "prediction")
points(pd[,2]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC1_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
plot((modMic1_Vol$fitted.values)~MOD_MICS_VOL$MIC1_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 1 ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_VOL$MIC1_VOL)
TAB = data.frame(OBSERVADO = MOD_MICS_VOL$MIC1_VOL, PREDITO = modMic1_Vol$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic1_Vol.csv")
dev.off()


jpeg("Dados/Analise/modMic2_Vol.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC2_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
modMic2_Vol = lm((MOD_MICS_VOL$MIC2_VOL) ~ MOD_MICS_VOL$CONTADOR)
resum = summary(modMic2_Vol)
plot(MOD_MICS_VOL$MIC2_VOL~MOD_MICS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nMictório 2 x Contador", ylab = "Consumo mictório 2 (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic2_Vol, lwd = 2, col = "red")
MOD_MICS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modMic2_Vol, MOD_MICS_VOL, interval = "prediction")
points(pd[,2]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC2_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
plot((modMic2_Vol$fitted.values)~MOD_MICS_VOL$MIC2_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 2 ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_VOL$MIC2_VOL)
TAB = data.frame(OBSERVADO = MOD_MICS_VOL$MIC2_VOL, PREDITO = modMic2_Vol$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic2_Vol.csv")
dev.off()


jpeg("Dados/Analise/modMic3_Vol.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC3_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
modMic3_Vol = lm((MOD_MICS_VOL$MIC3_VOL) ~ MOD_MICS_VOL$CONTADOR)
resum = summary(modMic3_Vol)
plot(MOD_MICS_VOL$MIC3_VOL~MOD_MICS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nMictório 3 x Contador", ylab = "Consumo mictório 3 (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic3_Vol, lwd = 2, col = "red")
MOD_MICS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modMic3_Vol, MOD_MICS_VOL, interval = "prediction")
points(pd[,2]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MIC3_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
plot((modMic3_Vol$fitted.values)~MOD_MICS_VOL$MIC3_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 3 ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_VOL$MIC3_VOL)
TAB = data.frame(OBSERVADO = MOD_MICS_VOL$MIC3_VOL, PREDITO = modMic3_Vol$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic3_Vol.csv")
dev.off()

##TORNEIRAS E MICTORIOS----------------------

jpeg("Dados/Analise/modMICTORIO_VOL.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MICTORIO_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
modMICTORIO_VOL = lm((MOD_MICS_VOL$MICTORIO_VOL) ~ MOD_MICS_VOL$CONTADOR)
resum = summary(modMICTORIO_VOL)
plot(MOD_MICS_VOL$MICTORIO_VOL~MOD_MICS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nMictórios x Contador", ylab = "Consumo mictórios (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMICTORIO_VOL, lwd = 2, col = "red")
MOD_MICS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modMICTORIO_VOL, MOD_MICS_VOL, interval = "prediction")
points(pd[,2]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$MICTORIO_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
plot((modMICTORIO_VOL$fitted.values)~MOD_MICS_VOL$MICTORIO_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictórios ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_VOL$MICTORIO_VOL)
TAB = data.frame(OBSERVADO = MOD_MICS_VOL$MICTORIO_VOL, PREDITO = modMICTORIO_VOL$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMictorio_Vol.csv")
dev.off()


jpeg("Dados/Analise/modTORNEIRA_VOL.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$TORNEIRA_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
modTORNEIRA_VOL = lm((MOD_PIAS_VOL$TORNEIRA_VOL) ~ MOD_PIAS_VOL$CONTADOR)
resum = summary(modTORNEIRA_VOL)
plot(MOD_PIAS_VOL$TORNEIRA_VOL~MOD_PIAS_VOL$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - VOLUME (PREV. 95%)\nTorneiras x Contador", ylab = "Consumo torneiras (litros/dia)", xlab = "Quantidade de pessoas (dia)")
abline(modTORNEIRA_VOL, lwd = 2, col = "red")
MOD_PIAS_VOL <- data.frame(CONTADOR = 0:200)
pd = predict(modTORNEIRA_VOL, MOD_PIAS_VOL, interval = "prediction")
points(pd[,2]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_VOL$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_PIAS_VOL = VOL[(VOL$CONTADOR != 0 & VOL$TORNEIRA_VOL != 0),] #& VOL$MIC2_VOL != 0 & VOL$MIC3_VOL != 0),]
plot((modTORNEIRA_VOL$fitted.values)~MOD_PIAS_VOL$TORNEIRA_VOL, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneiras ~ Contador", ylab = "Consumo predito (litros/dia)", xlab = "Consumo observado (litros/dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_VOL$TORNEIRA_VOL)
TAB = data.frame(OBSERVADO = MOD_PIAS_VOL$TORNEIRA_VOL, PREDITO = modTORNEIRA_VOL$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modTorneira_Vol.csv")
dev.off()



##################################################
## MODELOS PARA EVENTOS USANDO APENAS O CONTADOR ####################################################
##################################################


jpeg("Dados/Analise/modPia4_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$PIA4_QTD != 0),]
modPia4_QTD = lm((MOD_PIAS_QTD$PIA4_QTD) ~ MOD_PIAS_QTD$CONTADOR)
resum = summary(modPia4_QTD)
plot(MOD_PIAS_QTD$PIA4_QTD~MOD_PIAS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nTorneira 1 x Contador", ylab = "Qtd. eventos torneira 1 (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modPia4_QTD, lwd = 2, col = "red")
MOD_PIAS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modPia4_QTD, MOD_PIAS_QTD, interval = "prediction")
points(pd[,2]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$PIA4_QTD != 0),]
plot((modPia4_QTD$fitted.values)~MOD_PIAS_QTD$PIA4_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneira 1 ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_QTD$PIA4_QTD)
TAB = data.frame(OBSERVADO = MOD_PIAS_QTD$PIA4_QTD, PREDITO = modPia4_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modPia4_QTD.csv")
dev.off()


jpeg("Dados/Analise/modPia5_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$PIA5_QTD != 0),]
modPia5_QTD = lm((MOD_PIAS_QTD$PIA5_QTD) ~ MOD_PIAS_QTD$CONTADOR)
resum = summary(modPia5_QTD)
plot(MOD_PIAS_QTD$PIA5_QTD~MOD_PIAS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nTorneira 2 x Contador", ylab = "Qtd. eventos torneira 2 (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modPia5_QTD, lwd = 2, col = "red")
MOD_PIAS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modPia5_QTD, MOD_PIAS_QTD, interval = "prediction")
points(pd[,2]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$PIA5_QTD != 0),]
plot((modPia5_QTD$fitted.values)~MOD_PIAS_QTD$PIA5_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneira 2 ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_QTD$PIA5_QTD)
TAB = data.frame(OBSERVADO = MOD_PIAS_QTD$PIA5_QTD, PREDITO = modPia5_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modPia5_QTD.csv")
dev.off()


jpeg("Dados/Analise/modMic1_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC1_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
modMic1_QTD = lm((MOD_MICS_QTD$MIC1_QTD) ~ MOD_MICS_QTD$CONTADOR)
resum = summary(modMic1_QTD)
plot(MOD_MICS_QTD$MIC1_QTD~MOD_MICS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nMictório 1 x Contador", ylab = "Qtd. eventos mictório 1 (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic1_QTD, lwd = 2, col = "red")
MOD_MICS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modMic1_QTD, MOD_MICS_QTD, interval = "prediction")
points(pd[,2]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC1_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
plot((modMic1_QTD$fitted.values)~MOD_MICS_QTD$MIC1_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 1 ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_QTD$MIC1_QTD)
TAB = data.frame(OBSERVADO = MOD_MICS_QTD$MIC1_QTD, PREDITO = modMic1_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic1_QTD.csv")
dev.off()


jpeg("Dados/Analise/modMic2_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC2_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
modMic2_QTD = lm((MOD_MICS_QTD$MIC2_QTD) ~ MOD_MICS_QTD$CONTADOR)
resum = summary(modMic2_QTD)
plot(MOD_MICS_QTD$MIC2_QTD~MOD_MICS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nMictório 2 x Contador", ylab = "Qtd. eventos mictório 2 (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic2_QTD, lwd = 2, col = "red")
MOD_MICS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modMic2_QTD, MOD_MICS_QTD, interval = "prediction")
points(pd[,2]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC2_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
plot((modMic2_QTD$fitted.values)~MOD_MICS_QTD$MIC2_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 2 ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_QTD$MIC2_QTD)
TAB = data.frame(OBSERVADO = MOD_MICS_QTD$MIC2_QTD, PREDITO = modMic2_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic2_QTD.csv")
dev.off()


jpeg("Dados/Analise/modMic3_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC3_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
modMic3_QTD = lm((MOD_MICS_QTD$MIC3_QTD) ~ MOD_MICS_QTD$CONTADOR)
resum = summary(modMic3_QTD)
plot(MOD_MICS_QTD$MIC3_QTD~MOD_MICS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nMictório 3 x Contador", ylab = "Qtd. eventos mictório 3 (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMic3_QTD, lwd = 2, col = "red")
MOD_MICS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modMic3_QTD, MOD_MICS_QTD, interval = "prediction")
points(pd[,2]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MIC3_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
plot((modMic3_QTD$fitted.values)~MOD_MICS_QTD$MIC3_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictório 3 ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_QTD$MIC3_QTD)
TAB = data.frame(OBSERVADO = MOD_MICS_QTD$MIC3_QTD, PREDITO = modMic3_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMic3_QTD.csv")
dev.off()

##TORNEIRAS E MICTORIOS----------------------

jpeg("Dados/Analise/modMICTORIO_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MICTORIO_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
modMICTORIO_QTD = lm((MOD_MICS_QTD$MICTORIO_QTD) ~ MOD_MICS_QTD$CONTADOR)
resum = summary(modMICTORIO_QTD)
plot(MOD_MICS_QTD$MICTORIO_QTD~MOD_MICS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nMictórios x Contador", ylab = "Qtd. eventos mictórios (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modMICTORIO_QTD, lwd = 2, col = "red")
MOD_MICS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modMICTORIO_QTD, MOD_MICS_QTD, interval = "prediction")
points(pd[,2]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_MICS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)
MOD_MICS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$MICTORIO_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
plot((modMICTORIO_QTD$fitted.values)~MOD_MICS_QTD$MICTORIO_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nMictórios ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_MICS_QTD$MICTORIO_QTD)
TAB = data.frame(OBSERVADO = MOD_MICS_QTD$MICTORIO_QTD, PREDITO = modMICTORIO_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modMictorio_QTD.csv")
dev.off()


jpeg("Dados/Analise/modTORNEIRA_QTD.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$TORNEIRA_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
modTORNEIRA_QTD = lm((MOD_PIAS_QTD$TORNEIRA_QTD) ~ MOD_PIAS_QTD$CONTADOR)
resum = summary(modTORNEIRA_QTD)
plot(MOD_PIAS_QTD$TORNEIRA_QTD~MOD_PIAS_QTD$CONTADOR, pch = 4, lwd = 2,
     main = "AJUSTE DO MODELO - EVENTO (PREV. 95%)\nTorneiras x Contador", ylab = "Qtd. eventos torneiras (dia)", xlab = "Quantidade de pessoas (dia)")
abline(modTORNEIRA_QTD, lwd = 2, col = "red")
MOD_PIAS_QTD <- data.frame(CONTADOR = 0:200)
pd = predict(modTORNEIRA_QTD, MOD_PIAS_QTD, interval = "prediction")
points(pd[,2]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
points(pd[,3]~MOD_PIAS_QTD$CONTADOR, type = "l", lwd = 2, lty = 2, col = "lightgray")
legend("bottomright", legend = c(paste0("Coef. angular: ", round(resum$coefficients[2,1],2)), paste0("Qualid. Ajuste: ", round(resum$adj.r.squared,2)),
                                 paste0("Sigma resíd.: ", round(resum$sigma ,2))), cex = 0.8)

MOD_PIAS_QTD = QTD[(QTD$CONTADOR != 0 & QTD$TORNEIRA_QTD != 0),] #& QTD$MIC2_QTD != 0 & QTD$MIC3_QTD != 0),]
plot((modTORNEIRA_QTD$fitted.values)~MOD_PIAS_QTD$TORNEIRA_QTD, pch = 4, lwd = 2,
     main = "VALORES PREDITOS x AJUSTADOS\nTorneiras ~ Contador", ylab = "Qtd. eventos predito (dia)", xlab = "Qtd. eventos observado (dia)")
abline(0,1, lwd = 2, col = "red")
ERRO = ((-resum$residuals)/MOD_PIAS_QTD$TORNEIRA_QTD)
TAB = data.frame(OBSERVADO = MOD_PIAS_QTD$TORNEIRA_QTD, PREDITO = modTORNEIRA_QTD$fitted.values, ERRO_ABS = (-resum$residuals), ERRO)
write.csv2(TAB, "Dados/Analise/modTorneira_QTD.csv")
dev.off()




head(test)
mod = lm((test$VOLUME)~test$DIA_SEM+port$VOLUME+test$PERIODO)
summary(mod)
plot(mod)
pred = ifelse(predict(mod)<0, 0, predict(mod))
plot(test$VOLUME~test$DIA_SEM)
plot(test$VOLUME~port$VOLUME)
hist(test$VOLUME, breaks = 50)
mean((test$VOLUME-pred)/pred)

dat = round(data.frame(REAL = test$VOLUME, PREDITO = pred, DIFERENCA = test$VOLUME-pred),2)
sum(dat[,3])
mean()

par(mfrow = c(1,1))


dad = subset(tabHora, tabHora$LOCAL=="torneira" & dad$VOLUME>0)

medias = aggregate(dad$VOLUME, list(hour(dad$HORA_REF)), mean2)
medias = aggregate(dad$VOLUME, list((dad$DIA_SEM)), mean2)
medias = cbind(medias, aggregate(dad$VOLUME, list((dad$DIA_SEM)), sd2)$x)

#medias = cbind(medias, aggregate(dad$VOLUME, list(hour(dad$HORA_REF)), sd2)$x)

names(medias) = c("HORA", "MEDIA", "DESVIO")

par(mfrow = c(1,1))
plot(dad$VOLUME~dad$DIA_SEM)

round(medias, 2)

plot()

for(i in unique(tabPeriodo$PERIODO)){
  tab = subset(tabPeriodo, tabPeriodo$PERIODO == i)
  tabpia = subset(tab, tab$LOCAL == "torneira")
  tabmic = subset(tab, tab$LOCAL == "mictorio")
  
  
  plot(tabpia$VOLUME~tabmic$VOLUME, main = i, ylim = c(min(tabPeriodo$VOLUME), 40), xlim = c(min(tabPeriodo$VOLUME), 25))
}



tabPessEv = subset(tabEv, tabEv$LOCAL == "pess")

pia = subset(tabEv, tabEv$LOCAL=="pia4" | tabEv$LOCAL=="pia5"| tabEv$LOCAL=="pia6")
piaMan = subset(pia, date(pia$HORA_REF)==today()-1 & pia$PERIODO=="Manhã")

write.csv2(piaMan, "piasExp.csv", row.names = F, fileEncoding = "WINDOWS-1252")

pia4 = subset(tabEv, tabEv$LOCAL=="pia4" & tabEv$VOLUME>0 & tabEv$VOLUME<5)
pia5 = subset(tabEv, tabEv$LOCAL=="pia5" & tabEv$VOLUME>0 & tabEv$VOLUME<5)
pia6 = subset(tabEv, tabEv$LOCAL=="pia6" & tabEv$VOLUME>0 & tabEv$VOLUME<5) #& tabEv$VOLUME<5)
mic1 = subset(tabEv, tabEv$LOCAL=="mic1" & tabEv$VOLUME>0 & tabEv$VOLUME<5)
mic2 = subset(tabEv, tabEv$LOCAL=="mic2" & tabEv$VOLUME>0 & tabEv$VOLUME<5)
mic3 = subset(tabEv, tabEv$LOCAL=="mic3" & tabEv$VOLUME>0 & tabEv$VOLUME<5)
pias = rbind(pia4, pia5, pia6)
mics = rbind(mic1, mic2, mic3)

Ap_volume = pia4$VOLUME
TabResPia4 = data.frame(Local = "Torneira 1",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))
Ap_volume = pia5$VOLUME
TabResPia5 = data.frame(Local = "Torneira 2",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

Ap_volume = pia6$VOLUME
TabResPia6 = data.frame(Local = "Torneira 3",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

Ap_volume = mic1$VOLUME
TabResMic1 = data.frame(Local = "Mictório 1",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

Ap_volume = mic2$VOLUME
TabResMic2 = data.frame(Local = "Mictório 2",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

Ap_volume = mic3$VOLUME
TabResMic3 = data.frame(Local = "Mictório 3",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))


Ap_volume = pias$VOLUME
TabResPias = data.frame(Local = "Torneiras",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

Ap_volume = mics$VOLUME
TabResMics = data.frame(Local = "Mictórios",
                        N = length(Ap_volume),
                        Media = mean(Ap_volume),
                        DesvioPadrao = sd(Ap_volume),
                        ErroPadrao = sd(Ap_volume)/sqrt(length(Ap_volume)),
                        Minimo = min(Ap_volume),
                        Q1 = quantile(Ap_volume, .25),
                        Mediana = median(Ap_volume),
                        Q3 = quantile(Ap_volume, .75),
                        Maximo = max(Ap_volume))

geral = rbind(TabResPia4, TabResPia5, TabResPia6, TabResPias,
              TabResMic1, TabResMic2, TabResMic3, TabResMics)
write.csv2(geral, "Dados/Analise/resumo.csv", row.names = F, fileEncoding = "WINDOWS-1252")

pontos
head(tabEv)

jpeg("Dados/Analise/pia1-2.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
hist(pia4$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDA TORNEIRA 1", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(pia4$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(pia4$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(pia4$VOLUME), 2)), paste0("Média: ", round(mean(pia4$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )

hist(pia5$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDA TORNEIRA 2", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(pia5$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(pia5$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(pia5$VOLUME), 2)), paste0("Média: ", round(mean(pia5$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )
dev.off()

jpeg("Dados/Analise/pia3-total.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
hist(pia6$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDA TORNEIRA 3", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(pia6$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(pia6$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(pia6$VOLUME), 2)), paste0("Média: ", round(mean(pia6$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )

hist(pias$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDAS TORNEIRAS", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(pias$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(pias$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(pias$VOLUME), 2)), paste0("Média: ", round(mean(pias$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )
dev.off()


jpeg("Dados/Analise/mic1-2.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
hist(mic1$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDO MICTÓRIO 1", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(mic1$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(mic1$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(mic1$VOLUME), 2)), paste0("Média: ", round(mean(mic1$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )

hist(mic2$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDO MICTÓRIO 2", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(mic2$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(mic2$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(mic2$VOLUME), 2)), paste0("Média: ", round(mean(mic2$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )
dev.off()


jpeg("Dados/Analise/mic3-total.jpeg",width = 800, height = 400)
par(mfrow = c(1,2))
hist(mic3$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDO MICTÓRIO 3", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(mic3$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(mic3$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(mic3$VOLUME), 2)), paste0("Média: ", round(mean(mic3$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )

hist(mics$VOLUME, main = "HISTOGRAMA DO VOLUME POR EVENTO\nDOS MICTÓRIOS", xlab = "Volume (litros) por evento", ylab = "Frequência", col = "lightgray", breaks = 20, xlim = c(0,5))
abline(v=median(mics$VOLUME), col = "red", lwd= 2, lty = 2)
abline(v=mean(mics$VOLUME), col = "green", lwd= 2, lty = 3)
legend("topright", legend = c(paste0("Mediana: ", round(median(mics$VOLUME), 2)), paste0("Média: ", round(mean(mics$VOLUME),2))), cex = 1., lty = c(2,3), lwd = 2, col = c("red", "green"), box.lwd = 0 )
dev.off()


par(mfrow = c(2,4))

minVol = 0.2
volBaixo = subset(pia4, pia4$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDA TORNEIRA 1" )

volBaixo = subset(pia4, pia4$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDA TORNEIRA 2" )

volBaixo = subset(pia5, pia5$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDA TORNEIRA 3" )

volBaixo = subset(pia6, pia6$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDAS TORNEIRAS" )

volBaixo = subset(mic1, mic1$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDO MICTÓRIO 1" )

volBaixo = subset(mic2, mic2$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDO MICTÓRIO 2" )

volBaixo = subset(mic3, mic3$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDO MICTÓRIO 3" )

volBaixo = subset(mics, mics$VOLUME<minVol)
tab = table(hour(volBaixo$HORA_REF))
barplot(tab, xlab = "Horas dos dia", ylab = "Frequência", main = "DISTRIBUIÇÃO DOS VOLUMES BAIXOS\nDOS MICTÓRIOS" )





mean(pia4$VOLUME)
mean(pia5$VOLUME)
mean(pia6$VOLUME)


max(pia4$DURACAO)
subset(pia4, pia4$DURACAO > 20)
vaso = subset(tabEv, tabEv$LOCAL=="vaso")
hist(vaso$VOLUME)

# tabela = transfConsumoMedido_Ev(carregaConsumo(carregaPontos()))


tabDia = transfConsumoMedidoDia(tabHora)
micDia = subset(tabDia, tabDia$LOCAL=="mictorio")
pessDia = subset(tabDia, tabDia$LOCAL=="pess")
globDia = subset(tabDia, tabDia$LOCAL=="global")
tornDia = subset(tabDia, tabDia$LOCAL=="torneira")
vasoDia = subset(tabDia, tabDia$LOCAL=="vaso")

tabXY = merge(pessDia, micDia, "DIA", all = T)
tabAnalise = subset(tabXY, tabXY$VOLUME.x>0 & tabXY$VOLUME.y>0)
# tabAnalise = subset(tabAnalise, (abs(erro)<100))
# tabAnalise[which(abs(erro)>100),]
mod1 = lm(VOLUME.y ~ (VOLUME.x) -1, data = tabAnalise)
modSum1 = summary(mod1)

plot(tabAnalise$VOLUME.y ~ (tabAnalise$VOLUME.x), pch = 4, lwd= 2,
     xlab = "Quantidade de pessoas por dia", ylab = "Volume usado nos mictórios (litros/dia)", main = "REGRESSÃO DO USO DE ÁGUA NOS MICTÓRIOS\nPELA QUANTIDADE DE PESSOAS")
abline(mod1, col = 2,  lwd= 2)
legend("bottomright", legend = c(paste0("Coeficiente angular: ", round(mod1$coefficients,2)), paste0("Qualidade do ajuste: ", round(modSum1$adj.r.squared,2)),
       paste0("Sigma dos resíduos: ", round(modSum1$sigma ,2))), cex = 0.8)
volPred = seq(0, 200, 0.5)
pred = predict(mod1, data.frame(VOLUME.x = volPred), interval="predict", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
pred = predict(mod1, data.frame(VOLUME.x = volPred), interval="confidence", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")


tabXY = merge(pessDia, tornDia, "DIA", all = T)
tabAnalise = subset(tabXY, tabXY$VOLUME.x>0 & tabXY$VOLUME.y>0)
mod2 = lm(VOLUME.y ~ (VOLUME.x) -1, data = tabAnalise)
modSum2 = summary(mod2)
plot(tabAnalise$VOLUME.y ~ (tabAnalise$VOLUME.x), pch = 4, lwd= 2,
     xlab = "Quantidade de pessoas por dia", ylab = "Volume usado nas torneiras (litros/dia)", main = "REGRESSÃO DO USO DE ÁGUA NAS TORNEIRAS\nPELA QUANTIDADE DE PESSOAS")
abline(mod2, col = 2,  lwd= 2)
legend("bottomright", legend = c(paste0("Coeficiente angular: ", round(mod2$coefficients,2)), paste0("Qualidade do ajuste: ", round(modSum2$adj.r.squared,2)),
                                 paste0("Sigma dos resíduos: ", round(modSum2$sigma ,2))), cex = 0.8)
volPred = seq(0, 200, 0.5)
pred = predict(mod2, data.frame(VOLUME.x = volPred), interval="predict", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
pred = predict(mod2, data.frame(VOLUME.x = volPred), interval="confidence", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")

#legend("bottomright", legend = c(paste0("Coeficiente angular: ", round(mod2$coefficients,2)), paste0("Qualidade do ajuste: ", round(modSum2$adj.r.squared,2))), cex = 0.8)


tabXY = merge(pessDia, vasoDia, "DIA", all = T)
tabAnalise = subset(tabXY, tabXY$VOLUME.x>0 & tabXY$VOLUME.y>0)
mod2 = lm(VOLUME.y ~ (VOLUME.x) -1, data = tabAnalise)
modSum2 = summary(mod2)
plot(tabAnalise$VOLUME.y ~ (tabAnalise$VOLUME.x), pch = 4, lwd= 2,
     xlab = "Quantidade de pessoas por dia", ylab = "Volume usado nos vasos sanitários (litros/dia)", main = "REGRESSÃO DO USO DE ÁGUA NOS VASOS\nPELA QUANTIDADE DE PESSOAS")
abline(mod2, col = 2,  lwd= 2)
legend("bottomright", legend = c(paste0("Coeficiente angular: ", round(mod2$coefficients,2)), paste0("Qualidade do ajuste: ", round(modSum2$adj.r.squared,2)),
                                 paste0("Sigma dos resíduos: ", round(modSum2$sigma ,2))), cex = 0.8)
volPred = seq(0, 200, 0.5)
pred = predict(mod2, data.frame(VOLUME.x = volPred), interval="predict", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "lightblue")
pred = predict(mod2, data.frame(VOLUME.x = volPred), interval="confidence", level = .95)
points(pred[,2]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")
points(pred[,3]~volPred, lty = 2,  lwd= 2, type = "l", col = "blue")




#plot((-tabAnalise$VOLUME.y+mod$fitted.values)/tabAnalise$VOLUME.y*100, ylab = "ERRO (%)", xlab = "Observações")
#abline(0,0)

# plot(, ylab = "ERRO (%)", xlab = "Observações")
# erro = ((mod$residuals)/tabAnalise$VOLUME.y*100)
# plot(erro~tabAnalise$VOLUME.y,  ylab = "ERRO (%)", xlab = "Observações")
# abline(0,0)
# mean(erro)
# 
# plot(erro~tabAnalise$VOLUME.x,  ylab = "ERRO (%)", xlab = "Observações")
# abline(0,0)
# 
# (mean(mod$coefficients * tabAnalise$VOLUME.x)-mean(tabAnalise$VOLUME.y))/mean(tabAnalise$VOLUME.y)*100
