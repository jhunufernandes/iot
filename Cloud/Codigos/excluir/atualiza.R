setwd("/home/laftos/Projetos/Monitoramento")

source("Codigos/pacotes.R")
source("Codigos/func_auxiliar.R")
source("Codigos/func_carregaDados.R")
source("Codigos/func_corrigeDados.R")

temp = tempfile()
download.file("https://api.pcloud.com/getpubzip?code=kZgca07ZSsqq7pm6YhHTclNLFWcUVSH0auz7", temp)
unzip(temp,exdir="Dados")
unlink(temp)

# data <- read.table(unz(temp, "a1.dat"))


inicio = as.Date("2017-11-14")
fim = as.Date(now())
tabEvento = transfConsumoMedido_Ev(carregaConsumo(carregaPontos()))
tabHoraImp = transfConsumoMedidoHora(tabEvento, inicio , fim, TRUE)
tabHora = transfConsumoMedidoHora(tabEvento, inicio , fim, FALSE)

saveRDS(tabEvento, "Dados/dadosEvento.rds")
saveRDS(tabHora, "Dados/dadosHora.rds")
saveRDS(tabHoraImp, "Dados/dadosHoraImp.rds")

#unique(tab$LOCAL)
#subset(tab, tab$LOCAL=="pess")

#dad = inputVolume(tabHora)
